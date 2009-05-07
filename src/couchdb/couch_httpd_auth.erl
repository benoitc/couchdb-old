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

bin2int(Bin) ->
    L = 8 * size(Bin),
    <<Int:L>> = Bin,
    Int.

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $A+(N-10).

int(C) when $0 =< C, C =< $9 ->
    C - $0;
int(C) when $A =< C, C =< $F ->
    C - $A + 10;
int(C) when $a =< C, C =< $f ->
    C - $a + 10.
    
to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].
 
list_to_hexstr([]) -> 
    [];
list_to_hexstr([H|T]) ->
    to_hex(H) ++ list_to_hexstr(T).

bin_to_hexstr(Bin) ->
    list_to_hexstr(binary_to_list(Bin)).

hexstr_to_bin(S) ->
    list_to_binary(hexstr_to_list(S)).

hexstr_to_list([X,Y|T]) ->
    [int(X)*16 + int(Y) | hexstr_to_list(T)];
hexstr_to_list([]) ->
    [].

% Login handler for per-db user db
handle_login_req(#httpd{method='POST', mochi_req=MochiReq}=Req, #db{}=Db) ->
    {ok, #doc{body={AuthDoc}}} = open_auth_doc(Db),
    Secret = proplists:get_value(<<"secret">>, AuthDoc, nil),
    % 256-bit key
    % 115b8b692e0e045692cf280b436735c77a5a9e8a9e7ed56c965f87db5b2a2ece3
    Mod = <<33:32/integer, 16#01:8,
            16#15b8b692:32, 16#e0e04569:32, 16#2cf280b4:32, 16#36735c77:32,
            16#a5a9e8a9:32, 16#e7ed56c9:32, 16#65f87db5:32, 16#b2a2ece3:32>>,
    ModInt = crypto:erlint(Mod),
    Generator = crypto:mpint(2),
    % k = H(N, g), in SRP-6a
    ModHex = ?l2b(integer_to_list(ModInt, 16)),
    GeneratorHex = ?l2b(integer_to_list(crypto:erlint(Generator), 16)),
    Multiplier = bin2int(crypto:sha(<<ModHex/binary, GeneratorHex/binary>>)),
    ReqBody = MochiReq:recv_body(),
    {Props} = ?JSON_DECODE(ReqBody),
    UserName = proplists:get_value(<<"username">>, Props, nil),
    User = case get_user(Db, UserName) of
    nil -> [];
    Result -> Result
    end,
    UserSalt = proplists:get_value(<<"salt">>, User, <<>>),
    Verifier = case proplists:get_value(<<"verifier">>, User, nil) of
    nil -> crypto:erlint(crypto:rand_uniform(crypto:mpint(2), Mod));
    V -> list_to_integer(?b2l(V), 16)
    end,
    case proplists:get_value(<<"M1">>, Props, nil) of
    nil ->
        % Generate random number b, 1 < b < n
        PrivKey = crypto:rand_uniform(crypto:mpint(2), Mod),
        % Compute ephemeral public key B = kv + g^b
        B = Multiplier * Verifier + crypto:erlint(crypto:mod_exp(Generator, PrivKey, Mod)),
        PrivKeySize = size(PrivKey),
        <<EncryptionKey:PrivKeySize/binary, _Rest/binary>> = Secret,
        EncryptedPrivKey = crypto:exor(PrivKey, EncryptionKey), 
        {NowMS, NowS, _} = erlang:now(),
        TimeStamp = NowMS * 1000000 + NowS,
        send_json(Req#httpd{req_body=ReqBody}, 200, [],
            {[{s, UserSalt},
              {<<"B">>, ?l2b(integer_to_list(B, 16))},
              {<<"b">>, ?l2b(bin_to_hexstr(EncryptedPrivKey))},
              {<<"timestamp">>, ?l2b(integer_to_list(TimeStamp, 16))}]});
    M1 ->
        A = proplists:get_value(<<"A">>, Props, <<>>),
        B = proplists:get_value(<<"B">>, Props, <<>>),
        AInt = list_to_integer(?b2l(A), 16),
        % Abort if A == 0 (mod N)
        case AInt rem ModInt of
        0 ->
            throw({unauthorized, <<"Invalid public key sent by client.">>});
        _Else ->
            % u = H(A, B)
            Scrambler = crypto:mpint(bin2int(crypto:sha(<<A/binary, B/binary>>))),
            EncryptedPrivKey = hexstr_to_bin(?b2l(proplists:get_value(<<"b">>, Props, nil))),
            PrivKeySize = size(EncryptedPrivKey),
            <<EncryptionKey:PrivKeySize/binary, _Rest/binary>> = Secret,
            PrivKey = crypto:exor(EncryptedPrivKey, EncryptionKey),
            % S = (Av^u) ^ b
            S = crypto:erlint(crypto:mod_exp(
                    crypto:mpint(AInt *
                        crypto:erlint(
                            crypto:mod_exp(crypto:mpint(Verifier),
                            Scrambler, Mod))),
                    PrivKey, Mod)),
            % K = H(S)
            K = ?l2b(bin_to_hexstr(crypto:sha(?l2b(integer_to_list(S, 16))))),
            % M[1] = H(A, B, K)
            M1Verify = ?l2b(bin_to_hexstr(crypto:sha(<<A/binary, B/binary, K/binary>>))),
            TimeStamp = list_to_integer(?b2l(proplists:get_value(<<"timestamp">>, Props, nil)), 16),
            {NowMS, NowS, _} = erlang:now(),
            CurrentTime = NowMS * 1000000 + NowS,
            Timeout = 600,
            if
            M1 == M1Verify andalso CurrentTime < TimeStamp + Timeout ->
                % M[2] = H(A, M[1], K)
                M2 = ?l2b(bin_to_hexstr(crypto:sha(<<A/binary, M1/binary, K/binary>>))),
                Headers = [cookie_auth_cookie(?b2l(UserName), <<Secret/binary, UserSalt/binary>>, TimeStamp)],
                send_json(Req#httpd{req_body=ReqBody}, 200, Headers,
                    {[{<<"M2">>, M2}]});
            true ->
                throw({unauthorized, <<"Name or password is incorrect.">>})
            end
        end
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
