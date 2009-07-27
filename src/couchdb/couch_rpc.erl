%% @author Benoît Chesneau <benoitc@e-engura.org>
%% @copyright 2009 Benoît Chesneau.
%%
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%%
%%      http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.
%%
%% Some code based on hovercraft from  J Chris Anderson <jchris@couch.io>
%% Copyright 2009 J Chris Anderson <jchris@couch.io> under Apache 2 License.
%%

-module(couch_rpc).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([server_info/0, db_info/1, create_db/1, delete_db/1, 
         save_doc/2, open_doc/2, save_bulk/2, delete_doc/2,
         query_view/3, query_view/4]).

-include("couch_db.hrl").

-define(ADMIN_USER_CTX, {user_ctx, #user_ctx{roles=[<<"_admin">>]}}).

start_link() ->
     gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init([]) ->
    process_flag(trap_exit, true),
    {ok, nil}.
    
server_info() ->
    gen_server:call(?MODULE, server_info).

db_info(DbName) ->
    gen_server:call(?MODULE, {db_info, DbName}).

create_db(DbName) ->
    gen_server:call(?MODULE, {create_db, DbName}).
    
delete_db(DbName) ->
    gen_server:call(?MODULE, {delete_db, DbName}).
    
save_doc(DbName, Doc) ->
    gen_server:call(?MODULE, {save_doc, DbName, Doc}).
    
open_doc(DbName, DocId) ->
    gen_server:call(?MODULE, {open_doc, DbName, DocId}).
    
save_bulk(DbName, Docs) ->
    gen_server:call(?MODULE, {save_bulk, DbName, Docs}).
    
delete_doc(DbName, Doc) ->
    gen_server:call(?MODULE, {delete_doc, DbName, Doc}).


query_view(DbName, DesignName, ViewName) ->
    query_view(DbName, DesignName, ViewName, #view_query_args{}).
    
query_view(DbName, DesignName, ViewName, #view_query_args{}=QueryArgs) ->
    gen_server:call(?MODULE, {query_view, DbName, DesignName, ViewName, QueryArgs}).

handle_call(server_info, _From, State) ->
    ServerInfo = [
        {couchdb, <<"Welcome">>},
        {version, list_to_binary(couch_server:get_version())}
    ],
    {reply, ServerInfo, State};

handle_call({create_db, DbName}, _From, State) ->
    {ok, Db} = open_db(DbName),
    R = couch_db:get_db_info(Db),
    {reply, R, State};
    
handle_call({delete_db, DbName, Options}, _From, State) ->
    R = case couch_server:delete(DbName, Options) of
    ok ->
        {ok, deleted};
    Error ->
        {error, Error}
    end,
    {reply, R, State};
            
handle_call({db_info, DbName}, _From, State) ->
    {ok, Db} = open_db(DbName),
    R = couch_db:get_db_info(Db),
    {reply, R, State};
    
handle_call({save_doc, DbName, Doc}, _From, State) ->
    {ok, Db} = open_db(DbName),
    CouchDoc = ejson_to_couch_doc(Doc),
    {ok, Rev} = couch_db:update_doc(Db, CouchDoc, []),
    R = {ok, {[{id, CouchDoc#doc.id}, {rev, couch_doc:rev_to_str(Rev)}]}},
    {reply, R, State};
    
handle_call({open_doc, DbName, DocId}, _From, State) ->
    {ok, Db} = open_db(DbName),
    
    CouchDoc = try couch_httpd_db:couch_doc_open(Db, DocId, nil, [])
        catch
            _:Error -> {error, Error}
        end,
    R = case CouchDoc of
        {error, Error1} -> Error1;
        _ -> {ok, couch_doc:to_json_obj(CouchDoc, [])}
    end,
    {reply, R, State};
    
handle_call({save_bulk, DbName, Docs}, _From, State) ->
    {ok, Db} = open_db(DbName),
    CouchDocs = [ejson_to_couch_doc(EJsonDoc) || EJsonDoc <- Docs],
    {ok, Results} = couch_db:update_docs(Db, CouchDocs),
    R = {ok, lists:zipwith(fun couch_httpd_db:update_doc_result_to_json/2,
        CouchDocs, Results)},
    {reply, R, State};
    
handle_call({delete_doc, DbName, {DocProps}}, From, State) ->
     handle_call({save_doc, DbName, {[{<<"_deleted">>, true}|DocProps]}}, From, State);
     
handle_call({query_view,DbName, DesignName, ViewName, QueryArgs}, _From, State) ->
    R = query_view1(DbName, DesignName, ViewName, QueryArgs),
    {reply, R, State}.
     
         
handle_cast(_Msg, State) ->
    {noreply, State}.    
    
handle_info({'EXIT', Reason}, State) ->
    {reply, Reason, State};
    
handle_info(_Info, State) ->
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
terminate(_Reason, _State) ->
    ok.
    

%% Internal Api
open_db(DbName) ->
    couch_db:open(DbName, [?ADMIN_USER_CTX]).
    

query_view1(DbName, DesignName, ViewName, #view_query_args{
            limit = Limit,
            skip = SkipCount,
            stale = Stale,
            direction = Dir,
            group_level = GroupLevel,
            start_key = StartKey,
            start_docid = StartDocId,
            end_key = EndKey,
            end_docid = EndDocId
        }=QueryArgs) ->
    {ok, Db} = open_db(DbName),
    % get view reference
    DesignId = <<"_design/", DesignName/binary>>,
    case couch_view:get_map_view(Db, DesignId, ViewName, Stale) of
        {ok, View, Group} ->
            {ok, RowCount} = couch_view:get_row_count(View),
            Start = {StartKey, StartDocId},
            FoldlFun = couch_httpd_view:make_view_fold_fun(nil, 
                QueryArgs, <<"">>, Db, RowCount, 
                #view_fold_helper_funs{
                    reduce_count = fun couch_view:reduce_to_count/1,
                    start_response = fun start_map_view_fold_fun/5,
                    send_row = fun map_row_fold_fun/5
                }),
            FoldAccInit = {Limit, SkipCount, undefined, [], nil},
            {ViewFoldAcc, Offset} = case couch_view:fold(View, Start, Dir, FoldlFun, FoldAccInit) of
                {ok, {_, _, undefined, _, Offset1}} -> {[], Offset1};
                {ok, {_, _, _, ViewFoldAcc1, Offset2}} -> {ViewFoldAcc1, Offset2}
                end,
            {ok, {RowCount, Offset, ViewFoldAcc}};
        {not_found, Reason} ->
            case couch_view:get_reduce_view(Db, DesignId, ViewName, Stale) of
                {ok, View, Group} ->
                    {ok, GroupRowsFun, RespFun} = 
                        couch_httpd_view:make_reduce_fold_funs(nil, 
                            GroupLevel, QueryArgs, <<"">>, 
                            #reduce_fold_helper_funs{
                                start_response = fun start_reduce_view_fold_fun/3,
                                send_row = fun reduce_row_fold_fun/3
                            }),
                    FoldAccInit = {Limit, SkipCount, undefined, []},
                    {ok, {_, _, _, AccResult}} = couch_view:fold_reduce(View, Dir, {StartKey, StartDocId}, 
                        {EndKey, EndDocId}, GroupRowsFun, RespFun, FoldAccInit),
                    {ok, AccResult};
                _ ->
                    throw({not_found, Reason})
            end
    end.

    
ejson_to_couch_doc({DocProps}) ->
    Doc = case proplists:get_value(<<"_id">>, DocProps) of
        undefined ->
            DocId = couch_util:new_uuid(),
            {[{<<"_id">>, DocId}|DocProps]};
        _DocId ->
            {DocProps}
        end,
    couch_doc:from_json_obj(Doc).
    
start_map_view_fold_fun(_Req, _Etag, _RowCount, Offset, Acc) ->
    {ok, nil, []}.

map_row_fold_fun(_Resp, Db, {{Key, DocId}, Value}, _IncludeDocs, Acc) ->
    Acc1 = [{{Key, DocId}, Value}|Acc],
    {ok, Acc1}.

start_reduce_view_fold_fun(Req, Etag, _Acc0) ->
    {ok, nil, []}.

reduce_row_fold_fun(_Resp, {Key, Value}, Acc) ->
    Acc1 = [{Key, Value}|Acc],
    {ok, Acc1}.