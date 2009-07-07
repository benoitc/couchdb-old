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
        do_proxy_request(Req, {RawPath, Headers, Method, ReqBody}, _ActionKey, DestPath1);
    {_ActionKey, "", _RelativePath} ->
        RedirectPath = couch_httpd:path(Req) ++ "/",
        couch_httpd:send_redirect(Req, RedirectPath)
end;
handle_proxy_req(Req, _) ->
    couch_httpd:send_method_not_allowed(Req, "").
    
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
                             couch_httpd:send_json(Req, 502, {[{error, <<"Bad Gateway">>}, {reason, << "Bad redirection" >>}]})
                    end;    
                 false ->
                    RespHeaders1 = fix_location(RespHeaders, {SrcPath, DestPath}),
                    ?LOG_DEBUG("httpd ~p proxy response headers:~n ~p", [list_to_integer(Status), 
                                                                        RespHeaders]),
                    Body = list_to_binary(get_body(RespBody)),
                    {ok, Resp} = couch_httpd:start_chunked_response(Req, list_to_integer(Status), RespHeaders1),
                    couch_doc:bin_foldl(Body,
                            fun(BinSegment, _) -> couch_httpd:send_chunk(Resp, BinSegment) end,[]),
                    couch_httpd:send_chunk(Resp, "")
            end;
        {error, Reason} ->
            couch_httpd:send_json(Req, 502, {[{error, <<"Bad Gateway">>}, {reason, << Reason >>}]})
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