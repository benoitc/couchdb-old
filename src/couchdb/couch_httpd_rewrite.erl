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

-module(couch_httpd_rewrite).

-export([handle_rewrite_req/2]).

-include("couch_db.hrl").

handle_rewrite_req(#httpd{
        mochi_req=MochiReq,
        method='GET',
        path_parts=[DbName, _Design, DesignName, _Rewrite | _Rest]
    }=Req, Db) ->
    DesignId = <<"_design/", DesignName/binary>>,
    % TODO cache the design doc and only reload when it changes
    #doc{body={Props}} = couch_httpd_db:couch_doc_open(Db, DesignId, nil, []),
    DispatchList = [json_to_dispatch_list(X) || {X} <- proplists:get_value(<<"rewrites">>, Props, [])],
    {"/" ++ Path, _, _} = mochiweb_util:urlsplit_path(MochiReq:get(raw_path)),
    [_, _, _, _ | MatchPath] = string:tokens(Path, "/"),
    Dispatched = webmachine_dispatcher:dispatch(string:join(MatchPath, "/"), DispatchList),
    case Dispatched of
    {no_dispatch_match, _} ->
        couch_httpd:send_error(Req, 404, <<"rewrite_error">>, <<"Invalid path.">>);
    {_Action, MatchOpts, PathTokens, Bindings, _AppRoot, _StringPath} ->
        TargetPath = lists:flatten([case X of
            '*' -> [?l2b(mochiweb_util:unquote(Y)) || Y <- PathTokens];
            Y when is_atom(Y) -> [?l2b(mochiweb_util:unquote(proplists:get_value(Y, Bindings, "")))];
            Y -> [?l2b(mochiweb_util:unquote(Y))] end || X <- MatchOpts]),
        ?LOG_DEBUG("Internal rewrite to: ~p", [TargetPath]),
        couch_httpd_db:handle_request(Req#httpd{
            path_parts=[DbName | TargetPath]})
    end.

json_to_dispatch_list(Props) ->
    PathTermList = [json_to_erlang(X)
        || X <- proplists:get_value(<<"match">>, Props, [])],
    Mod = not_used,
    MatchOpts = [json_to_erlang(X)
        || X <- proplists:get_value(<<"rewrite">>, Props, [])],
    {PathTermList, Mod, MatchOpts}.

json_to_erlang(<<String/binary>>) ->
    AtomSize = size(String) - 2,
    case String of
        <<"<", Atom:AtomSize/binary, ">">> -> binary_to_atom(Atom, utf8);
        _Else -> ?b2l(String)
    end.
