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

-module(couch_httpd_proxy_handler).

-export([handle_proxy_req/2]).
    
-include("couch_db.hrl").
-include("../ibrowse/ibrowse.hrl").

handle_proxy_req(#httpd{mochi_req=MochiReq}=Req, DestPath) ->
    DestPath1 = parse_dest_path(DestPath, Req),
    "/" ++ UrlPath = couch_httpd:path(Req),
    case couch_httpd:partition(UrlPath) of
    {_ActionKey, "/", RelativePath} ->
        RawPath = lists:append([DestPath1, "/", RelativePath,
            case couch_httpd:qs(Req) of
                [] -> [];
                Qs -> "?" ++ mochiweb_util:urlencode(Qs)
            end]),
        ?LOG_DEBUG("Proxy path ~s", [RawPath]),
        Headers = clean_request_headers(
                    mochiweb_headers:to_list(MochiReq:get(headers))),
        Method = mochiweb_to_ibrowse_method(MochiReq:get(method)),
        ReqBody = get_body(couch_httpd:body(Req)),
        case do_proxy_request(RawPath, Headers, Method, ReqBody) of
            {redirect, RedirectUrl, _} ->
                case mochiweb_util:partition(RedirectUrl, DestPath1) of
                     {"", _, RelPath} ->
                          RedirectPath = lists:append(["/", _ActionKey, RelPath]),
                          ?LOG_DEBUG("redirect to ~s", [RedirectPath]),
                          couch_httpd:send_redirect(Req, RedirectPath);
                     {_, "", ""} ->
                         couch_httpd:send_json(Req, 502, {[{error, <<"Bad Gateway">>}, {reason, << "Bad redirection" >>}]})
                end;
            {ok, RcvFun, {Status, RespHeaders}} ->
                %%RespHeaders1 = fix_location(RespHeaders, {_ActionKey, DestPath1}),
                ?LOG_DEBUG("proxy response status ~p", [Status]),
                {ok, Resp} = couch_httpd:start_chunked_response(Req, Status, RespHeaders),
                proxy_respond(RcvFun, Resp);
            {error, ResponseCode} ->
                couch_httpd:send_error(Req, ResponseCode);
            _ ->
                couch_httpd:send_json(Req, 502, {[{error, <<"Bad Gateway">>}]})
        end;
                
    {_ActionKey, "", _RelativePath} ->
        RedirectPath = couch_httpd:path(Req) ++ "/",
        couch_httpd:send_redirect(Req, RedirectPath)
end;
handle_proxy_req(Req, _) ->
    couch_httpd:send_method_not_allowed(Req, "").

proxy_respond(Source, State) when is_function(Source) ->
    proxy_respond({Source}, State);
proxy_respond({Source}, State) when is_function(Source) ->
    try 
        proxy_respond1(Source, Source(), State)
    catch 
        _:_ -> couch_httpd:send_chunk(State, "")
    end;
proxy_respond(Body, State) ->
    couch_doc:bin_foldl(Body,
        fun(BinSegment, _) -> couch_httpd:send_chunk(State, BinSegment) end,[]),
    couch_httpd:send_chunk(State, "").

proxy_respond1(Source, Resp, State) ->
    case Resp of
    {ok, ""} ->
        couch_httpd:send_chunk(State, "");
	{ok, Data} ->
	    couch_httpd:send_chunk(State, Data),
	    proxy_respond({Source}, State);
	_ ->
	    couch_httpd:send_chunk(State, "")
    end.

proxy_loop(ReqId, Conn) ->
    couch_util:should_flush(),
    receive
        {From, {set_req_id, NewId}} ->
            %% we learn the ReqId to listen for
            From ! {self(), {ok, NewId}},
            proxy_loop(NewId, Conn);
        {ibrowse_async_headers, ReqId, Status, Headers} ->
            %% we got header, give the controlling process a chance to react
            receive
                {From, gimme_status} ->
                    %% send status/headers to controller
                    From ! {self(), {status, Status, Headers}},
                    receive
                        {From, continue} ->
                            %% normal case
                            proxy_loop(ReqId, Conn);
                        {From, fail} ->
                            %% error, failure code
                            ?LOG_ERROR(
                                "streaming proxy body failed with status ~p",
                                [Status]),
                            catch ibrowse:stop_worker_process(Conn),
                            exit(proxy_request_failed);
                        {From, stop_ok} ->
                            %% stop looping, controller will start a new loop
                            catch ibrowse:stop_worker_process(Conn),
                            stop_ok
                    end
            end,
            proxy_loop(ReqId, Conn);
        {ibrowse_async_response, ReqId, {chunk_start,_}} ->
            proxy_loop(ReqId, Conn);
        {ibrowse_async_response, ReqId, chunk_end} ->
            proxy_loop(ReqId, Conn);
        {ibrowse_async_response, ReqId, {error, Err}} ->
            ?LOG_ERROR("streaming attachment failed with ~p", [Err]),
            catch ibrowse:stop_worker_process(Conn),
            exit(proxy_request_failed);
        {ibrowse_async_response, ReqId, Data} ->
            receive {From, gimme_data} ->  From ! {self(), Data} end,
            proxy_loop(ReqId, Conn);
        {ibrowse_async_response_end, ReqId} ->
            receive {From, gimme_data} ->  From ! {self(), ""} end,
            catch ibrowse:stop_worker_process(Conn),
            exit(normal)
    end.  
    
do_proxy_request(Url, Headers, Method, Body) ->
    do_proxy_request(Url, Headers, Method, Body, 3, 1000).

do_proxy_request(Url, _Headers, _Method, _Body, 0, _Pause) ->
    {proxy_request_failed, ?l2b(["failed to fetch response ", Url])};
    
do_proxy_request(Url, Headers, Method, Body, Retries, Pause) ->
    #url{host=Host, port=Port} = ibrowse_lib:parse_url(Url),
    {ok, Conn} = ibrowse:spawn_link_worker_process(Host, Port),
    Pid = spawn_link(fun() -> proxy_loop(nil, Conn) end),
    Opts = [{stream_to, Pid}, {response_format, binary}],
    ReqId = 
    case ibrowse:send_req_direct(Conn, Url, Headers, Method, Body, Opts, infinity) of
    {ibrowse_req_id, X} ->
             X;
    {error, Reason} ->
        ?LOG_INFO("error while fetching body due to {error, ~p } on ~s", [Reason, Url]),
        catch ibrowse:stop_worker_process(Conn),
        timer:sleep(Pause),
        do_proxy_request(Url, Headers, Method, Body,
        Retries-1, 2*Pause)
    end,
    Pid ! {self(), {set_req_id, ReqId}},
    receive
    {Pid, {ok, ReqId}} ->
        ok;
    {'EXIT', Pid, _Reason} ->
        catch ibrowse:stop_worker_process(Conn),
        timer:sleep(Pause),
        do_proxy_request(Url, Headers, Method, Body,
            Retries-1, 2*Pause)
    end,
    Pid ! {self(), gimme_status},
    receive
        {'EXIT', Pid, proxy_request_failed} ->
             catch ibrowse:stop_worker_process(Conn),
             do_proxy_request(Url, Headers, Method, Body,
                     Retries-1, Pause);
        {Pid, {status, StreamStatus, StreamHeaders}} ->
            ?LOG_DEBUG("streaming proxy response Status ~p Headers ~p",
                [StreamStatus, StreamHeaders]),
            ResponseCode = list_to_integer(StreamStatus),
            if
            ResponseCode >= 200, ResponseCode < 300 ->
                Pid ! {self(), continue},
                {ok, fun() ->
                    Pid ! {self(), gimme_data},
                    receive
                        {Pid, Data} ->
                            {ok, Data};
                        {Pid, ""} ->
                            eof;
                        {'EXIT', Pid, proxy_request_failed} ->
                            throw(proxy_response_failed)
                    end
                end, {ResponseCode, StreamHeaders}};
             ResponseCode >= 300, ResponseCode < 400 ->
                 Pid ! {self(), stop_ok},
                 RedirectUrl = mochiweb_headers:get_value("Location",
                     mochiweb_headers:make(StreamHeaders)),
                 {redirect, RedirectUrl, StreamHeaders};
             ResponseCode >= 400, ResponseCode < 500 ->
                 ?LOG_ERROR("streaming proxy response failed with code ~p: ~s",
                     [ResponseCode, Url]),
                 Pid ! {self(), fail},
                 {error, ResponseCode};
             ResponseCode == 500 ->
                 % an error... log and retry
                 ?LOG_INFO("retrying proxy request in ~p " ++
                     "seconds due to 500 response: ~s", [Pause/1000, Url]),
                 Pid ! {self(), fail},
                 catch ibrowse:stop_worker_process(Conn),
                 timer:sleep(Pause),
                 do_proxy_request(Url, Headers, Method, Body,
                     Retries-1, 2*Pause)
        end
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
  
parse_dest_path(DestPath, Req) when is_binary(DestPath) ->
    parse_dest_path(?b2l(DestPath), Req);
parse_dest_path(DestPath, Req) when is_list(DestPath) ->
    case mochiweb_util:partition(DestPath, "/") of
        {[], "/", _} -> remove_trailing(couch_httpd:absolute_uri(Req, DestPath));
        _ -> remove_trailing(DestPath)
    end.
    
remove_trailing(Path) ->
    case lists:last(Path) of
        $/ -> lists:sublist(Path, 1, length(Path) -1);
        _  -> Path
    end.
    
get_body(Body) ->
    case Body of
        undefined -> [];
        Other -> Other
    end.