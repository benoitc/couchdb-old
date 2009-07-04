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
    {Action, {MatchOpts, Extra}, PathTokens, Bindings, AppRoot, StringPath} ->
        ?LOG_DEBUG("Successful Dispatch: ~p", [{Action, MatchOpts, PathTokens, Bindings, AppRoot, StringPath}]),
        TargetPath = [DbName | lists:flatten([case X of
            '*' -> [?l2b(mochiweb_util:unquote(Y)) || Y <- PathTokens];
            Y when is_atom(Y) -> [?l2b(mochiweb_util:unquote(proplists:get_value(Y, Bindings, "")))];
            Y -> [?l2b(mochiweb_util:unquote(Y))] end || X <- MatchOpts])],
        {_Path, QueryString, _Fragment} = mochiweb_util:urlsplit_path(MochiReq:get(raw_path)),
        QueryStringParsed = mochiweb_util:parse_qs(QueryString),
        ?LOG_DEBUG("Internal rewrite to: ~p", [TargetPath]),
        RawPath = string:join([?b2l(X) || X <- TargetPath], "/") ++ "?" ++ mochiweb_util:urlencode(QueryStringParsed ++ [
                {K, ?b2l(iolist_to_binary(?JSON_ENCODE(V)))} || {K, V} <- replace(Extra, Bindings)]),
        ?LOG_DEBUG("Internal rewrite to: ~p", [RawPath]),
        couch_httpd_db:handle_request(Req#httpd{
            path_parts=TargetPath,
            mochi_req=mochiweb_request:new(MochiReq:get(socket), MochiReq:get(method), RawPath, MochiReq:get(version), MochiReq:get(headers))
        })
    end.

replace({X, Y}, Bindings) ->
    {replace(X, Bindings), replace(Y, Bindings)};
replace([X | Rest], Bindings) ->
    [replace(X, Bindings) | replace(Rest, Bindings)];
replace(X, Bindings) when is_atom(X) ->
    ?l2b(mochiweb_util:unquote(proplists:get_value(X, Bindings, "")));
replace(X, Bindings) -> X.

json_to_dispatch_list(Props) ->
    {ok, SlashRE} = re:compile(<<"\\/">>),
    [Match | _MatchRest] = proplists:get_value(<<"match">>, Props, []),
    [Rewrite | QueryParams] = proplists:get_value(<<"rewrite">>, Props, []),
    PathTermList = json_to_erlang(re:split(Match, SlashRE)),
    MatchOpts = {
        json_to_erlang(re:split(Rewrite, SlashRE)),
        case QueryParams of
            [{QueryList}] -> json_to_erlang(QueryList);
            _Else -> []
        end
    },
    {PathTermList, not_used, MatchOpts}.

json_to_erlang([X | Rest]) -> [json_to_erlang(X) | json_to_erlang(Rest)];
json_to_erlang([]) -> [];
json_to_erlang({K, V}) -> {json_to_erlang(K), json_to_erlang(V)};
json_to_erlang(<<String/binary>>) ->
    AtomSize = size(String) - 2,
    case String of
        <<"<", Atom:AtomSize/binary, ">">> -> binary_to_atom(Atom, utf8);
        _Else -> ?b2l(String)
    end.
