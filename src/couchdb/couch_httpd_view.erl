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

-module(couch_httpd_view).
-include("couch_db.hrl").

-export([handle_view_req/2,handle_temp_view_req/2]).

-export([parse_view_query/1,parse_view_query/2,make_view_fold_fun/5,
    finish_view_fold/3, view_row_obj/3]).

-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,end_json_response/1]).

design_doc_view(Req, Db, Id, ViewName, Keys) ->
    #view_query_args{
        stale = Stale,
        reduce = Reduce
    } = QueryArgs = parse_view_query(Req, Keys),
    DesignId = <<"_design/", Id/binary>>,
    case couch_view:get_map_view(Db, DesignId, ViewName, Stale) of
    {ok, View} ->    
        output_map_view(Req, View, Db, QueryArgs, Keys);
    {not_found, Reason} ->
        case couch_view:get_reduce_view(Db, DesignId, ViewName, Stale) of
        {ok, ReduceView} ->
            parse_view_query(Req, Keys, true), % just for validation
            case Reduce of
            false ->
                MapView = couch_view:extract_map_view(ReduceView),
                output_map_view(Req, MapView, Db, QueryArgs, Keys);
            _ ->
                output_reduce_view(Req, ReduceView, QueryArgs, Keys)
            end;
        _ ->
            throw({not_found, Reason})
        end
    end.

handle_view_req(#httpd{method='GET',path_parts=[_,_, Id, ViewName]}=Req, Db) ->
    design_doc_view(Req, Db, Id, ViewName, nil);

handle_view_req(#httpd{method='POST',path_parts=[_,_, Id, ViewName]}=Req, Db) ->
    {Props} = couch_httpd:json_body(Req),
    Keys = proplists:get_value(<<"keys">>, Props, nil),
    design_doc_view(Req, Db, Id, ViewName, Keys);

handle_view_req(Req, _Db) ->
    send_method_not_allowed(Req, "GET,POST,HEAD").

handle_temp_view_req(#httpd{method='POST'}=Req, Db) ->
    QueryArgs = parse_view_query(Req),

    case couch_httpd:primary_header_value(Req, "content-type") of
        undefined -> ok;
        "application/json" -> ok;
        Else -> throw({incorrect_mime_type, Else})
    end,
    {Props} = couch_httpd:json_body(Req),
    Language = proplists:get_value(<<"language">>, Props, <<"javascript">>),
    {DesignOptions} = proplists:get_value(<<"options">>, Props, {[]}),
    MapSrc = proplists:get_value(<<"map">>, Props),
    Keys = proplists:get_value(<<"keys">>, Props, nil),
    case proplists:get_value(<<"reduce">>, Props, null) of
    null ->
        {ok, View} = couch_view:get_temp_map_view(Db, Language, 
            DesignOptions, MapSrc),
        output_map_view(Req, View, Db, QueryArgs, Keys);
    RedSrc ->
        {ok, View} = couch_view:get_temp_reduce_view(Db, Language, 
            DesignOptions, MapSrc, RedSrc),
        output_reduce_view(Req, View, QueryArgs, Keys)
    end;

handle_temp_view_req(Req, _Db) ->
    send_method_not_allowed(Req, "POST").

output_map_view(Req, View, Db, QueryArgs, nil) ->
    #view_query_args{
        limit = Limit,
        direction = Dir,
        skip = SkipCount,
        start_key = StartKey,
        start_docid = StartDocId
    } = QueryArgs,
    {ok, RowCount} = couch_view:get_row_count(View),
    Start = {StartKey, StartDocId},
    FoldlFun = make_view_fold_fun(Req, QueryArgs, Db, RowCount, #view_fold_helper_funs{reduce_count=fun couch_view:reduce_to_count/1}),
    FoldAccInit = {Limit, SkipCount, undefined, []},
    FoldResult = couch_view:fold(View, Start, Dir, FoldlFun, FoldAccInit),
    finish_view_fold(Req, RowCount, FoldResult);
    
output_map_view(Req, View, Db, QueryArgs, Keys) ->
    #view_query_args{
        limit = Limit,
        direction = Dir,
        skip = SkipCount,
        start_docid = StartDocId
    } = QueryArgs,
    {ok, RowCount} = couch_view:get_row_count(View),
    FoldAccInit = {Limit, SkipCount, undefined, []},
    FoldResult = lists:foldl(
        fun(Key, {ok, FoldAcc}) ->
            Start = {Key, StartDocId},
            FoldlFun = make_view_fold_fun(Req,
                QueryArgs#view_query_args{
                    start_key = Key,
                    end_key = Key
                }, Db, RowCount, 
                #view_fold_helper_funs{
                    reduce_count = fun couch_view:reduce_to_count/1
                }),
            couch_view:fold(View, Start, Dir, FoldlFun, FoldAcc)
        end, {ok, FoldAccInit}, Keys),
    finish_view_fold(Req, RowCount, FoldResult).

output_reduce_view(Req, View, QueryArgs, nil) ->
    #view_query_args{
        start_key = StartKey,
        end_key = EndKey,
        limit = Limit,
        skip = Skip,
        direction = Dir,
        start_docid = StartDocId,
        end_docid = EndDocId,
        group_level = GroupLevel
    } = QueryArgs,
    {ok, Resp} = start_json_response(Req, 200),
    {ok, GroupRowsFun, RespFun} = make_reduce_fold_funs(Resp, GroupLevel),
    send_chunk(Resp, "{\"rows\":["),
    {ok, _} = couch_view:fold_reduce(View, Dir, {StartKey, StartDocId}, 
        {EndKey, EndDocId}, GroupRowsFun, RespFun, {"", Skip, Limit}),
    send_chunk(Resp, "]}"),
    end_json_response(Resp);
    
output_reduce_view(Req, View, QueryArgs, Keys) ->
    #view_query_args{
        limit = Limit,
        skip = Skip,
        direction = Dir,
        start_docid = StartDocId,
        end_docid = EndDocId,
        group_level = GroupLevel
    } = QueryArgs,
    {ok, Resp} = start_json_response(Req, 200),
    {ok, GroupRowsFun, RespFun} = make_reduce_fold_funs(Resp, GroupLevel),
    send_chunk(Resp, "{\"rows\":["),
    lists:foldl(
        fun(Key, AccSeparator) ->
            {ok, {NewAcc, _, _}} = couch_view:fold_reduce(View, Dir, {Key, StartDocId}, 
                {Key, EndDocId}, GroupRowsFun, RespFun, 
                {AccSeparator, Skip, Limit}),
            NewAcc % Switch to comma
        end,
    "", Keys), % Start with no comma
    send_chunk(Resp, "]}"),
    end_json_response(Resp).
    
make_reduce_fold_funs(Resp, GroupLevel) ->
    GroupRowsFun =
        fun({_Key1,_}, {_Key2,_}) when GroupLevel == 0 ->
            true;
        ({Key1,_}, {Key2,_})
                when is_integer(GroupLevel) and is_list(Key1) and is_list(Key2) ->
            lists:sublist(Key1, GroupLevel) == lists:sublist(Key2, GroupLevel);
        ({Key1,_}, {Key2,_}) ->
            Key1 == Key2
        end,
    RespFun = fun(_Key, _Red, {AccSeparator,AccSkip,AccLimit}) when AccSkip > 0 ->
        {ok, {AccSeparator,AccSkip-1,AccLimit}};
    (_Key, _Red, {AccSeparator,0,AccLimit}) when AccLimit == 0 ->
        {stop, {AccSeparator,0,AccLimit}};
    (_Key, Red, {AccSeparator,0,AccLimit}) when GroupLevel == 0 ->
        Json = ?JSON_ENCODE({[{key, null}, {value, Red}]}),
        send_chunk(Resp, AccSeparator ++ Json),
        {ok, {",",0,AccLimit-1}};
    (Key, Red, {AccSeparator,0,AccLimit})
            when is_integer(GroupLevel) 
            andalso is_list(Key) ->
        Json = ?JSON_ENCODE(
            {[{key, lists:sublist(Key, GroupLevel)},{value, Red}]}),
        send_chunk(Resp, AccSeparator ++ Json),
        {ok, {",",0,AccLimit-1}};
    (Key, Red, {AccSeparator,0,AccLimit}) ->
        Json = ?JSON_ENCODE({[{key, Key}, {value, Red}]}),
        send_chunk(Resp, AccSeparator ++ Json),
        {ok, {",",0,AccLimit-1}}
    end,
    {ok, GroupRowsFun, RespFun}.
    



reverse_key_default(nil) -> {};
reverse_key_default({}) -> nil;
reverse_key_default(Key) -> Key.

parse_view_query(Req) ->
    parse_view_query(Req, nil, nil).
parse_view_query(Req, Keys) ->
    parse_view_query(Req, Keys, nil).
parse_view_query(Req, Keys, IsReduce) ->
    QueryList = couch_httpd:qs(Req),
    #view_query_args{
        group_level = GroupLevel
    } = QueryArgs = lists:foldl(fun({Key,Value}, Args) ->
        case {Key, Value} of
        {"", _} ->
            Args;
        {"key", Value} ->
            case Keys of
            nil ->
                JsonKey = ?JSON_DECODE(Value),
                Args#view_query_args{start_key=JsonKey,end_key=JsonKey};
            _ ->
                Msg = io_lib:format("Query parameter \"~s\" not compatible with multi key mode.", [Key]),
                throw({query_parse_error, Msg})
            end;
        {"startkey_docid", DocId} ->
            Args#view_query_args{start_docid=list_to_binary(DocId)};
        {"endkey_docid", DocId} ->
            Args#view_query_args{end_docid=list_to_binary(DocId)};
        {"startkey", Value} ->
            case Keys of
            nil ->
                Args#view_query_args{start_key=?JSON_DECODE(Value)};
            _ ->
                Msg = io_lib:format("Query parameter \"~s\" not compatible with multi key mode.", [Key]),
                throw({query_parse_error, Msg})
            end;
        {"endkey", Value} ->
            case Keys of
            nil ->
                Args#view_query_args{end_key=?JSON_DECODE(Value)};
            _ ->
                Msg = io_lib:format("Query parameter \"~s\" not compatible with multi key mode.", [Key]),
                throw({query_parse_error, Msg})
            end;
        {"limit", Value} ->
            case (catch list_to_integer(Value)) of
            Limit when is_integer(Limit) ->
                if Limit < 0 ->
                    Msg = io_lib:format("Limit must be a positive integer: limit=~s", [Value]),
                    throw({query_parse_error, Msg});
                true ->
                    Args#view_query_args{limit=Limit}
                end;
            _Error ->
                Msg = io_lib:format("Bad URL query value, number expected: limit=~s", [Value]),
                throw({query_parse_error, Msg})
            end;
        {"count", Value} ->
            throw({query_parse_error, "URL query parameter 'count' has been changed to 'limit'."});
        {"stale", "ok"} ->
            Args#view_query_args{stale=ok};
        {"update", "false"} ->
            throw({query_parse_error, "URL query parameter 'update=false' has been changed to 'stale=ok'."});
        {"descending", "true"} ->
            case Args#view_query_args.direction of
            fwd ->
                Args#view_query_args {
                    direction = rev,
                    start_key =
                        reverse_key_default(Args#view_query_args.start_key),
                    start_docid =
                        reverse_key_default(Args#view_query_args.start_docid),
                    end_key =
                        reverse_key_default(Args#view_query_args.end_key),
                    end_docid =
                        reverse_key_default(Args#view_query_args.end_docid)};
            _ ->
                Args %already reversed
            end;
        {"descending", "false"} ->
          % The descending=false behaviour is the default behaviour, so we
          % simpply ignore it. This is only for convenience when playing with
          % the HTTP API, so that a user doesn't get served an error when
          % flipping true to false in the descending option.
          Args;
        {"skip", Value} ->
            case (catch list_to_integer(Value)) of
            Limit when is_integer(Limit) ->
                Args#view_query_args{skip=Limit};
            _Error ->
                Msg = lists:flatten(io_lib:format(
                "Bad URL query value, number expected: skip=~s", [Value])),
                throw({query_parse_error, Msg})
            end;
        {"group", Value} ->
            case Value of
            "true" ->
                Args#view_query_args{group_level=exact};
            "false" ->
                Args#view_query_args{group_level=0};
            _ ->
                Msg = "Bad URL query value for 'group' expected \"true\" or \"false\".",
                throw({query_parse_error, Msg})
            end;
        {"group_level", LevelStr} ->
            case Keys of
            nil ->
                Args#view_query_args{group_level=list_to_integer(LevelStr)};
            _ ->
                Msg = lists:flatten(io_lib:format("Multi-key fetches for a reduce view must include group=true", [])),
                throw({query_parse_error, Msg})
            end;
        {"reduce", "true"} ->
            Args#view_query_args{reduce=true};
        {"reduce", "false"} ->
            Args#view_query_args{reduce=false};
        {"include_docs", Value} ->
            case Value of
            "true" ->
                Args#view_query_args{include_docs=true};
            "false" ->
                Args#view_query_args{include_docs=false};
            _ ->
                Msg1 = "Bad URL query value for 'include_docs' expected \"true\" or \"false\".",
                throw({query_parse_error, Msg1})
            end;
        {"format", _} ->
            % we just ignore format, so that JS can have it
            Args;
        _ -> % unknown key
            Msg = lists:flatten(io_lib:format(
                "Bad URL query key:~s", [Key])),
            throw({query_parse_error, Msg})
        end
    end, #view_query_args{}, QueryList),
    case IsReduce of
    true ->
        case QueryArgs#view_query_args.include_docs and QueryArgs#view_query_args.reduce of
        true ->
            ErrMsg = "Bad URL query key for reduce operation: include_docs",
            throw({query_parse_error, ErrMsg});
        _ ->
            ok
        end;
    _ ->
        ok
    end,
    case Keys of
    nil ->
        QueryArgs;
    _ ->
        case IsReduce of
        nil ->
            QueryArgs;
        _ ->
            case GroupLevel of
            exact ->
                QueryArgs;
            _ ->
                #view_query_args{reduce=OptReduce} = QueryArgs,
                case OptReduce of
                true ->
                    Msg = lists:flatten(io_lib:format(
                        "Multi-key fetches for a reduce view must include group=true", [])),
                    throw({query_parse_error, Msg});
                _ -> 
                    QueryArgs
                end
            end
        end
    end.

make_view_fold_fun(Req, QueryArgs, Db,
    TotalViewCount, HelperFuns) ->
    #view_query_args{
        end_key = EndKey,
        end_docid = EndDocId,
        direction = Dir
    } = QueryArgs,
    
    #view_fold_helper_funs{
        passed_end = PassedEndFun,
        start_response = StartRespFun,
        send_row = SendRowFun,
        reduce_count = ReduceCountFun
    } = apply_default_helper_funs(HelperFuns, {Dir, EndKey, EndDocId}),

    #view_query_args{
        include_docs = IncludeDocs
    } = QueryArgs,

    fun({{Key, DocId}, Value}, OffsetReds,
                      {AccLimit, AccSkip, Resp, AccRevRows}) ->
        PassedEnd = PassedEndFun(Key, DocId),
        case {PassedEnd, AccLimit, AccSkip, Resp} of
        {true, _, _, _} ->
            % The stop key has been passed, stop looping.
            {stop, {AccLimit, AccSkip, Resp, AccRevRows}};
        {_, 0, _, _} ->
            % we've done "limit" rows, stop foldling
            {stop, {0, 0, Resp, AccRevRows}};
        {_, _, AccSkip, _} when AccSkip > 0 ->
            {ok, {AccLimit, AccSkip - 1, Resp, AccRevRows}};
        {_, _, _, undefined} ->
            Offset = ReduceCountFun(OffsetReds),
            {ok, Resp2, BeginBody} = StartRespFun(Req, 200, 
                TotalViewCount, Offset),
            SendRowFun(Resp2, Db, 
                {{Key, DocId}, Value}, BeginBody, IncludeDocs),
            {ok, {AccLimit - 1, 0, Resp2, AccRevRows}};
        {_, AccLimit, _, Resp} when (AccLimit > 0) ->
            SendRowFun(Resp, Db, 
                {{Key, DocId}, Value}, nil, IncludeDocs),
            {ok, {AccLimit - 1, 0, Resp, AccRevRows}}
        end
    end.

apply_default_helper_funs(#view_fold_helper_funs{
    passed_end = PassedEnd,
    start_response = StartResp,
    send_row = SendRow
}=Helpers, {Dir, EndKey, EndDocId}) ->
    PassedEnd2 = case PassedEnd of
    undefined -> make_passed_end_fun(Dir, EndKey, EndDocId);
    _ -> PassedEnd
    end,

    StartResp2 = case StartResp of
    undefined -> fun json_view_start_resp/4;
    _ -> StartResp
    end,

    SendRow2 = case SendRow of
    undefined -> fun send_json_view_row/5;
    _ -> SendRow
    end,

    Helpers#view_fold_helper_funs{
        passed_end = PassedEnd2,
        start_response = StartResp2,
        send_row = SendRow2
    }.

make_passed_end_fun(Dir, EndKey, EndDocId) ->
    case Dir of
    fwd ->
        fun(ViewKey, ViewId) ->
            couch_view:less_json([EndKey, EndDocId], [ViewKey, ViewId])
        end;
    rev->
        fun(ViewKey, ViewId) ->
            couch_view:less_json([ViewKey, ViewId], [EndKey, EndDocId])
        end
    end.

json_view_start_resp(Req, Code, TotalViewCount, Offset) ->
    {ok, Resp} = couch_httpd:start_json_response(Req, Code),
    BeginBody = io_lib:format("{\"total_rows\":~w,\"offset\":~w,\"rows\":[\r\n",
            [TotalViewCount, Offset]),
    {ok, Resp, BeginBody}.

send_json_view_row(Resp, Db, {{Key, DocId}, Value}, RowFront, IncludeDocs) ->
    JsonObj = view_row_obj(Db, {{Key, DocId}, Value}, IncludeDocs),
    RowFront2 = case RowFront of
    nil -> ",\r\n";
    _ -> RowFront
    end,
    send_chunk(Resp, RowFront2 ++  ?JSON_ENCODE(JsonObj)).
    

view_row_obj(Db, {{Key, DocId}, Value}, IncludeDocs) ->
    case DocId of
    error ->
        {[{key, Key}, {error, Value}]};
    _ ->
        case IncludeDocs of
        true ->
            Rev = case Value of
            {Props} ->
                case is_list(Props) of
                true ->
                    proplists:get_value(<<"_rev">>, Props, []);
                _ ->
                    []
                end;
            _ ->
                []
            end,
            ?LOG_DEBUG("Include Doc: ~p ~p", [DocId, Rev]),
            case (catch couch_httpd_db:couch_doc_open(Db, DocId, Rev, [])) of
              {{not_found, missing}, _RevId} ->
                  {[{id, DocId}, {key, Key}, {value, Value}, {error, missing}]};
              {not_found, missing} ->
                  {[{id, DocId}, {key, Key}, {value, Value}, {error, missing}]};
              {not_found, deleted} ->
                  {[{id, DocId}, {key, Key}, {value, Value}]};
              Doc ->
                JsonDoc = couch_doc:to_json_obj(Doc, []), 
                {[{id, DocId}, {key, Key}, {value, Value}, {doc, JsonDoc}]}
            end;
        _ ->
            {[{id, DocId}, {key, Key}, {value, Value}]}
        end
    end.

finish_view_fold(Req, TotalRows, FoldResult) ->
    case FoldResult of
    {ok, {_, _, undefined, _}} ->
        % nothing found in the view, nothing has been returned
        % send empty view
        send_json(Req, 200, {[
            {total_rows, TotalRows},
            {rows, []}
        ]});
    {ok, {_, _, Resp, AccRevRows}} ->
        % end the view
        send_chunk(Resp, AccRevRows ++ "\r\n]}"),
        end_json_response(Resp);
    Error ->
        throw(Error)
    end.
