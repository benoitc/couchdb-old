% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_httpd_misc_handlers).

-export([handle_welcome_req/2,handle_favicon_req/2,handle_utils_dir_req/2,
    handle_all_dbs_req/1,handle_replicate_req/1,handle_restart_req/1,
    handle_uuids_req/1,handle_config_req/1,handle_log_req/1,
    handle_task_status_req/1,handle_sleep_req/1,handle_whoami_req/1]).
-export([increment_update_seq_req/2]).


-include("couch_db.hrl").

-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,end_json_response/1,
    start_chunked_response/3, send_error/4]).

% httpd global handlers

handle_welcome_req(#httpd{method='GET'}=Req, WelcomeMessage) ->
    send_json(Req, {[
        {couchdb, WelcomeMessage},
        {version, list_to_binary(couch_server:get_version())}
    ]});
handle_welcome_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_favicon_req(#httpd{method='GET'}=Req, DocumentRoot) ->
    couch_httpd:serve_file(Req, "favicon.ico", DocumentRoot);
handle_favicon_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_utils_dir_req(#httpd{method='GET'}=Req, DocumentRoot) ->
    "/" ++ UrlPath = couch_httpd:path(Req),
    case couch_httpd:partition(UrlPath) of
    {_ActionKey, "/", RelativePath} ->
        % GET /_utils/path or GET /_utils/
        couch_httpd:serve_file(Req, RelativePath, DocumentRoot);
    {_ActionKey, "", _RelativePath} ->
        % GET /_utils
        RedirectPath = couch_httpd:path(Req) ++ "/",
        couch_httpd:send_redirect(Req, RedirectPath)
    end;
handle_utils_dir_req(Req, _) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_sleep_req(#httpd{method='GET'}=Req) ->
    Time = list_to_integer(couch_httpd:qs_value(Req, "time")),
    receive snicklefart -> ok after Time -> ok end,
    send_json(Req, {[{ok, true}]});
handle_sleep_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_all_dbs_req(#httpd{method='GET'}=Req) ->
    {ok, DbNames} = couch_server:all_databases(),
    send_json(Req, DbNames);
handle_all_dbs_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").


handle_task_status_req(#httpd{method='GET'}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    % convert the list of prop lists to a list of json objects
    send_json(Req, [{Props} || Props <- couch_task_status:all()]);
handle_task_status_req(Req) ->
    send_method_not_allowed(Req, "GET,HEAD").

% add trailing slash if missing
fix_db_url(UrlBin) ->
    ?l2b(case lists:last(Url = ?b2l(UrlBin)) of
    $/ -> Url;
    _  -> Url ++ "/"
    end).


get_rep_endpoint(_Req, {Props}) ->
    Url = proplists:get_value(<<"url">>, Props),
    {BinHeaders} = proplists:get_value(<<"headers">>, Props, {[]}),
    {remote, fix_db_url(Url), [{?b2l(K),?b2l(V)} || {K,V} <- BinHeaders]};
get_rep_endpoint(_Req, <<"http://",_/binary>>=Url) ->
    {remote, fix_db_url(Url), []};
get_rep_endpoint(_Req, <<"https://",_/binary>>=Url) ->
    {remote, fix_db_url(Url), []};
get_rep_endpoint(#httpd{user_ctx=UserCtx}, <<DbName/binary>>) ->
    {local, DbName, UserCtx}.

handle_replicate_req(#httpd{method='POST'}=Req) ->
    {Props} = couch_httpd:json_body_obj(Req),
    Source = get_rep_endpoint(Req, proplists:get_value(<<"source">>, Props)),
    Target = get_rep_endpoint(Req, proplists:get_value(<<"target">>, Props)),
    case couch_rep:replicate(Source, Target) of
    {ok, {JsonResults}} ->
        send_json(Req, {[{ok, true} | JsonResults]});
    {error, {Type, Details}} ->
        send_json(Req, 500, {[{error, Type}, {reason, Details}]});
    {error, Reason} ->
        send_json(Req, 500, {[{error, Reason}]})
    end;
handle_replicate_req(Req) ->
    send_method_not_allowed(Req, "POST").


handle_restart_req(#httpd{method='POST'}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    couch_server_sup:restart_core_server(),
    send_json(Req, 200, {[{ok, true}]});
handle_restart_req(Req) ->
    send_method_not_allowed(Req, "POST").


handle_uuids_req(#httpd{method='GET'}=Req) ->
    Count = list_to_integer(couch_httpd:qs_value(Req, "count", "1")),
    CacheBustingHeaders = [{"Date", httpd_util:rfc1123_date()},
                           {"Cache-Control", "no-cache"},
                           {"Expires", "Fri, 01 Jan 1990 00:00:00 GMT"},  % Past date, ON PURPOSE!
                           {"Pragma", "no-cache"}],
    % generate the uuids
    UUIDs = [ couch_util:new_uuid() || _ <- lists:seq(1,Count)],
    % send a JSON response
    send_json(Req, 200, CacheBustingHeaders, {[{<<"uuids">>, UUIDs}]});
handle_uuids_req(Req) ->
    send_method_not_allowed(Req, "GET").


% Config request handler


% GET /_config/
% GET /_config
handle_config_req(#httpd{method='GET', path_parts=[_]}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    Grouped = lists:foldl(fun({{Section, Key}, Value}, Acc) ->
        case dict:is_key(Section, Acc) of
        true ->
            dict:append(Section, {list_to_binary(Key), list_to_binary(Value)}, Acc);
        false ->
            dict:store(Section, [{list_to_binary(Key), list_to_binary(Value)}], Acc)
        end
    end, dict:new(), couch_config:all()),
    KVs = dict:fold(fun(Section, Values, Acc) ->
        [{list_to_binary(Section), {Values}} | Acc]
    end, [], Grouped),
    send_json(Req, 200, {KVs});
% GET /_config/Section
handle_config_req(#httpd{method='GET', path_parts=[_,Section]}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    KVs = [{list_to_binary(Key), list_to_binary(Value)}
            || {Key, Value} <- couch_config:get(Section)],
    send_json(Req, 200, {KVs});
% PUT /_config/Section/Key
% "value"
handle_config_req(#httpd{method='PUT', path_parts=[_, Section, Key]}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    Value = couch_httpd:json_body(Req),
    Persist = couch_httpd:header_value(Req, "X-Couch-Persist") /= "false",
    OldValue = couch_config:get(Section, Key, ""),
    ok = couch_config:set(Section, Key, ?b2l(Value), Persist),
    send_json(Req, 200, list_to_binary(OldValue));
% GET /_config/Section/Key
handle_config_req(#httpd{method='GET', path_parts=[_, Section, Key]}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    case couch_config:get(Section, Key, null) of
    null ->
        throw({not_found, unknown_config_value});
    Value ->
        send_json(Req, 200, list_to_binary(Value))
    end;
% DELETE /_config/Section/Key
handle_config_req(#httpd{method='DELETE',path_parts=[_,Section,Key]}=Req) ->
    ok = couch_httpd:verify_is_server_admin(Req),
    case couch_config:get(Section, Key, null) of
    null ->
        throw({not_found, unknown_config_value});
    OldValue ->
        couch_config:delete(Section, Key),
        send_json(Req, 200, list_to_binary(OldValue))
    end;
handle_config_req(Req) ->
    send_method_not_allowed(Req, "GET,PUT,DELETE").


% httpd db handlers

increment_update_seq_req(#httpd{method='POST'}=Req, Db) ->
    {ok, NewSeq} = couch_db:increment_update_seq(Db),
    send_json(Req, {[{ok, true},
        {update_seq, NewSeq}
    ]});
increment_update_seq_req(Req, _Db) ->
    send_method_not_allowed(Req, "POST").

% httpd log handlers

handle_log_req(#httpd{method='GET'}=Req) ->
    Bytes = list_to_integer(couch_httpd:qs_value(Req, "bytes", "1000")),
    Offset = list_to_integer(couch_httpd:qs_value(Req, "offset", "0")),
    Chunk = couch_log:read(Bytes, Offset),
    {ok, Resp} = start_chunked_response(Req, 200, [
        % send a plaintext response
        {"Content-Type", "text/plain; charset=utf-8"},
        {"Content-Length", integer_to_list(length(Chunk))}
    ]),
    send_chunk(Resp, Chunk),
    send_chunk(Resp, "");
handle_log_req(Req) ->
    send_method_not_allowed(Req, "GET").


% whoami handler
handle_whoami_req(#httpd{method='GET', user_ctx=UserCtx}=Req) ->
    Name = UserCtx#user_ctx.name,
    Roles = UserCtx#user_ctx.roles,
    ForceLogin = couch_httpd:qs_value(Req, "force_login", "false"),
    case {Name, ForceLogin} of
        {null, "true"} ->
            throw({unauthorized, <<"Please login.">>});
        _False -> ok
    end,
    send_json(Req, {[
        {ok, true},
        {name, Name},
        {roles, Roles}
    ]});
handle_whoami_req(Req) ->
    send_method_not_allowed(Req, "GET").
    
handle_proxy_req(#httpd{mochi_req=MochiReq}=Req, DestPath) ->
    DestPath1 = fix_dest_path(DestPath, Req),
    "/" ++ UrlPath = couch_httpd:path(Req),
    case couch_httpd:partition(UrlPath) of
        {_ActionKey, "/", RelativePath} ->
            Path = lists:append([DestPath1, "/", RelativePath,
            case couch_httpd:qs(Req) of
                [] -> [];
                Qs -> "?" ++ mochiweb_util:urlencode(Qs)
            end]),
            ?LOG_DEBUG("Proxy path ~s", [Path]),
            Headers = clean_request_headers(
                        mochiweb_headers:to_list(MochiReq:get(headers))),
            Method = mochiweb_to_ibrowse_method(MochiReq:get(method)),
            
            ReqBody = get_body(couch_httpd:body(Req)),
            
            do_proxy_request(Req, {Path, Headers, Method, ReqBody}, _ActionKey, DestPath1);
        {_ActionKey, "", _RelativePath} ->
            RedirectPath = couch_httpd:path(Req) ++ "/",
            couch_httpd:send_redirect(Req, RedirectPath)
    end;
handle_proxy_req(Req, _) ->
    send_method_not_allowed(Req, "").
    
do_proxy_request(Req, {P, H, M, B}, SrcPath, DestPath) ->
    case ibrowse:send_req(P, H, M, B) of
         {ok, Status, RespHeaders, RespBody} ->
             case is_redirect(list_to_integer(Status)) of
                 true ->
                     {LocationHeader, _} = proplists:split(RespHeaders, ["Location"]),
                     [LocationHeader1|_] = LocationHeader,
                     Path = proplists:get_value("Location", LocationHeader1),
                     case mochiweb_util:partition(Path, DestPath) of
                         {"", _, RelPath} ->
                              RedirectPath = lists:append(["/", SrcPath, RelPath]),
                              ?LOG_DEBUG("redirect to ~s", [RedirectPath]),
                              couch_httpd:send_redirect(Req, RedirectPath);
                         {_, "", ""} ->
                             send_json(Req, 502, {[{error, <<"Bad Gateway">>}, {reason, << "Bad redirection" >>}]})
                    end;    
                 false ->
                    RespHeaders1 = fix_location(RespHeaders, {SrcPath, DestPath}),
                    ?LOG_DEBUG("httpd ~p proxy response headers:~n ~p", [list_to_integer(Status), 
                                                                        RespHeaders]),
                    Body = list_to_binary(get_body(RespBody)),
                    {ok, Resp} = start_chunked_response(Req, list_to_integer(Status), RespHeaders1),
                    couch_doc:bin_foldl(Body,
                            fun(BinSegment, _) -> send_chunk(Resp, BinSegment) end,[]),
                    send_chunk(Resp, "")
            end;
        {error, Reason} ->
            send_json(Req, 502, {[{error, <<"Bad Gateway">>}, {reason, << Reason >>}]})
    end.
          
is_redirect(Status) ->
    case Status of
        301 -> true;
        302 -> true;
        _ -> false
    end.
            
%% convert Req#httpd.method to ibrowse method atom 
mochiweb_to_ibrowse_method(Method) when is_list(Method) ->
    list_to_atom(string:to_lower(Method));
mochiweb_to_ibrowse_method(Method) when is_atom(Method) ->
    mochiweb_to_ibrowse_method(atom_to_list(Method)).
    
%% ibrowse will recalculate Host and Content-Length headers,
%% and will muck them up if they're manually specified
clean_request_headers(Headers) ->
    [{K,V} || {K,V} <- Headers,
              K /= 'Host', K /= 'Content-Length'].
             
%% replace location header 
fix_location([], _) -> [];
fix_location([{"Location", ProxyDataPath}|Rest],
             {DestPath, SrcPath}) ->
    ProxyPath = lists:nthtail(length(SrcPath), ProxyDataPath),
    [{"Location", DestPath++ProxyPath}|Rest];
fix_location([H|T], C) ->
    [H|fix_location(T, C)].
  
%% remove last trailing. 
%% TODO: find a faster way to do it
fix_dest_path(P, Req) ->
    P1 = case is_binary(P) of
        true -> binary_to_list(P);
        false -> P
    end,
    [C|_] = P1,
    
    Path = case [C] of
        "/" -> couch_httpd:absolute_uri(Req, P1);
        _ -> P1
    end,
    case lists:last(Path) of
        $/ -> 
            [_|P2] = lists:reverse(Path),
            lists:reverse(P2);
        _  -> Path
    end.
    
    
get_body(Body) ->
    case Body of
        undefined -> [];
        Other -> Other
    end.
        
