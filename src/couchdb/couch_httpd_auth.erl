% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_httpd_auth).
-include("couch_db.hrl").

-export([default_authentication_handler/1,special_test_authentication_handler/1]).
-export([cookie_authentication_handler/1,cookie_authentication_handler/2]).
-export([null_authentication_handler/1]).
-export([cookie_auth_header/2]).
-export([handle_login_req/2, handle_logout_req/2]).

-import(couch_httpd, [header_value/2, send_json/4, send_method_not_allowed/2]).
-import(erlang, [integer_to_list/2, list_to_integer/2]).

special_test_authentication_handler(Req) ->
    case header_value(Req, "WWW-Authenticate") of
    "X-Couch-Test-Auth " ++ NamePass ->
        % NamePass is a colon separated string: "joe schmoe:a password".
        [Name, Pass] = re:split(NamePass, ":", [{return, list}]),
        case {Name, Pass} of
        {"Jan Lehnardt", "apple"} -> ok;
        {"Christopher Lenz", "dog food"} -> ok;
        {"Noah Slater", "biggiesmalls endian"} -> ok;
        {"Chris Anderson", "mp3"} -> ok;
        {"Damien Katz", "pecan pie"} -> ok;
        {_, _} ->
            throw({unauthorized, <<"Name or password is incorrect.">>})
        end,
        Req#httpd{user_ctx=#user_ctx{name=?l2b(Name)}};
    _ ->
        % No X-Couch-Test-Auth credentials sent, give admin access so the
        % previous authentication can be restored after the test
        Req#httpd{user_ctx=#user_ctx{roles=[<<"_admin">>]}}
    end.

basic_username_pw(Req) ->
    case header_value(Req, "Authorization") of
    "Basic " ++ Base64Value ->
        case string:tokens(?b2l(couch_util:decodeBase64(Base64Value)),":") of
        [User, Pass] ->
            {User, Pass};
        [User] ->
            {User, ""};
        _ ->
            nil
        end;
    _ ->
        nil
    end.

default_authentication_handler(Req) ->
    case basic_username_pw(Req) of
    {User, Pass} ->
        case couch_server:is_admin(User, Pass) of
        true ->
            Req#httpd{user_ctx=#user_ctx{name=?l2b(User), roles=[<<"_admin">>]}};
        false ->
            throw({unauthorized, <<"Name or password is incorrect.">>})
        end;
    nil ->
        case couch_server:has_admins() of
        true ->
            Req#httpd{user_ctx=#user_ctx{}};
        false ->
            % if no admins, then everyone is admin! Yay, admin party!
            Req#httpd{user_ctx=#user_ctx{roles=[<<"_admin">>]}}
        end
    end.

null_authentication_handler(Req) ->
    Req#httpd{user_ctx=#user_ctx{roles=[<<"_admin">>]}}.

% Cookie auth handler using per-node user db
cookie_authentication_handler(Req, DbName) ->
    case cookie_auth_user(Req, ?l2b(DbName)) of
    % Fall back to default authentication handler
    nil -> default_authentication_handler(Req);
    Req2 -> Req2
    end.

% Cookie auth handler using per-db user db
cookie_authentication_handler(#httpd{path_parts=Path}=Req) ->
    case Path of
    [DbName|_] ->
        case cookie_auth_user(Req, DbName) of
        nil -> default_authentication_handler(Req);
        Req2 -> Req2
        end;
    _Else ->
        % Fall back to default authentication handler
        default_authentication_handler(Req)
    end.

get_user(Db, UserName) ->
    DesignId = <<"_design/_auth">>,
    ViewName = <<"users">>,
    case (catch couch_view:get_map_view(Db, DesignId, ViewName, nil)) of
    {ok, View, _Group} ->
        FoldlFun = fun
        ({{Key, _DocId}, Value}, _, nil) when Key == UserName -> {ok, Value};
        (_, _, Acc) -> {stop, Acc}
        end,
        case couch_view:fold(View, {UserName, nil}, fwd, FoldlFun, nil) of
        {ok, {Result}} -> Result;
        _Else -> nil
        end;
    {not_found, _Reason} ->
        case (catch couch_view:get_reduce_view(Db, DesignId, ViewName, nil)) of
        {ok, _ReduceView, _Group} ->
            not_implemented;
        {not_found, _Reason} ->
            nil
        end
    end.

open_auth_doc(Db) ->
    couch_db:open_doc(Db, <<"_design/_auth">>).

cookie_auth_user(_Req, undefined) -> nil;
cookie_auth_user(#httpd{mochi_req=MochiReq}=Req, DbName) ->
    case MochiReq:get_cookie_value("AuthSession") of
    undefined -> nil;
    [] -> nil;
    Cookie -> 
        case couch_db:open(DbName, [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}]) of
        {ok, Db} ->
            try
                AuthSession = couch_util:decodeBase64Url(Cookie),
                [User, TimeStr | HashParts] = string:tokens(?b2l(AuthSession), ":"),
                % Verify expiry and hash
                {NowMS, NowS, _} = erlang:now(),
                CurrentTime = NowMS * 1000000 + NowS,
                case open_auth_doc(Db) of
                {ok, #doc{body={AuthDoc}}} ->
                    case proplists:get_value(<<"secret">>, AuthDoc, nil) of
                    nil -> nil;
                    Secret ->
                        case get_user(Db, ?l2b(User)) of
                        nil -> nil;
                        Result ->
                            UserSalt = proplists:get_value(<<"salt">>, Result, <<"">>),
                            FullSecret = <<Secret/binary, UserSalt/binary>>,
                            ExpectedHash = crypto:sha_mac(FullSecret, User ++ ":" ++ TimeStr),
                            Hash = ?l2b(string:join(HashParts, ":")),
                            Timeout = 600,
                            case (catch list_to_integer(TimeStr, 16)) of
                            TimeStamp when CurrentTime < TimeStamp + Timeout andalso ExpectedHash == Hash ->
                                TimeLeft = TimeStamp + Timeout - CurrentTime,
                                ?LOG_DEBUG("Successful cookie auth as: ~p", [User]),
                                Req#httpd{user_ctx=#user_ctx{
                                    name=?l2b(User),
                                    roles=proplists:get_value(<<"roles">>, Result, [])
                                }, auth={FullSecret, TimeLeft < Timeout*0.9}};
                            _Else ->
                                nil
                            end
                        end
                    end;
                _Else ->
                    nil
                end
            after
                couch_db:close(Db)
            end;
        _Else ->
            nil
        end
    end.

cookie_auth_header(#httpd{user_ctx=#user_ctx{name=null}}, _Headers) -> [];
cookie_auth_header(#httpd{user_ctx=#user_ctx{name=User}, auth={Secret, true}}, Headers) ->
    % Note: we only set the AuthSession cookie if:
    %  * a valid AuthSession cookie has been received
    %  * we are outside a 10% timeout window
    %  * and if an AuthSession cookie hasn't already been set e.g. by a login
    %    or logout handler.
    % The login and logout handlers need to set the AuthSession cookie
    % themselves.
    case proplists:get_value("Set-Cookie", Headers) of
    undefined -> [];
    Cookie -> 
        case proplists:get_value("AuthSession",
            mochiweb_cookies:parse_cookie(Cookie), undefined) of
        undefined ->
            {NowMS, NowS, _} = erlang:now(),
            TimeStamp = NowMS * 1000000 + NowS,
            [cookie_auth_cookie(?b2l(User), Secret, TimeStamp)];
        _Else -> []
        end
    end;
cookie_auth_header(_Req, _Headers) -> [].

cookie_auth_cookie(User, Secret, TimeStamp) ->
    SessionData = User ++ ":" ++ integer_to_list(TimeStamp, 16),
    Hash = crypto:sha_mac(Secret, SessionData),
    mochiweb_cookies:cookie("AuthSession",
        couch_util:encodeBase64Url(SessionData ++ ":" ++ ?b2l(Hash)),
        [{path, "/"}, {http_only, true}]). % TODO add {secure, true} when SSL is detected

% Login handler for per-db user db
handle_login_req(#httpd{method='POST', mochi_req=MochiReq}=Req, #db{}=Db) ->
    ReqBody = MochiReq:recv_body(),
    Form = case MochiReq:get_primary_header_value("content-type") of
        "application/x-www-form-urlencoded" ++ _ ->
            mochiweb_util:parse_qs(ReqBody);
        _ ->
            []
    end,
    UserName = ?l2b(proplists:get_value("username", Form, "")),
    Password = ?l2b(proplists:get_value("password", Form, "")),
    User = case get_user(Db, UserName) of
        nil -> [];
        Result -> Result
    end,
    UserSalt = proplists:get_value(<<"salt">>, User, <<>>),
    PasswordHash = couch_util:encodeBase64(crypto:sha(<<UserSalt/binary, Password/binary>>)),
    case proplists:get_value(<<"password_sha">>, User, nil) of
        ExpectedHash when ExpectedHash == PasswordHash ->
            {ok, #doc{body={AuthDoc}}} = open_auth_doc(Db),
            Secret = proplists:get_value(<<"secret">>, AuthDoc, nil),
            {NowMS, NowS, _} = erlang:now(),
            CurrentTime = NowMS * 1000000 + NowS,
            Headers = [cookie_auth_cookie(?b2l(UserName), <<Secret/binary, UserSalt/binary>>, CurrentTime)],
            send_json(Req#httpd{req_body=ReqBody}, 200, Headers,
                {[{ok, true}]});
        _Else ->
            throw({unauthorized, <<"Name or password is incorrect.">>})
    end;
% Login handler for per-node user db
handle_login_req(#httpd{method='POST'}=Req, DbName) ->
    case couch_db:open(?l2b(DbName), [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}]) of
    {ok, Db} -> handle_login_req(Req, Db)
    end;
handle_login_req(Req, _Db) ->
    send_method_not_allowed(Req, "POST").

% Logout handler for per-db user db
handle_logout_req(#httpd{method='POST'}=Req, #db{}=_Db) ->
    send_json(Req, 200, [
        mochiweb_cookies:cookie("AuthSession", "", [{path, "/"}, {http_only, true}])
    ], {[{ok, true}]});
% Logout handler for per-node user db
handle_logout_req(#httpd{method='POST'}=Req, DbName) ->
    case couch_db:open(?l2b(DbName), [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}]) of
    {ok, Db} -> handle_logout_req(Req, Db)
    end;
handle_logout_req(Req, _Db) ->
    send_method_not_allowed(Req, "POST").
