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

-module(couch_batch_save).

-behaviour(gen_server).

%% API
-export([start_link/2, eventually_save_doc/3, commit_now/0, commit_now/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

-include("couch_db.hrl").

-record(batch_state, {
    batch_size=1000
    }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(BatchSize, BatchInterval) ->
    gen_server:start_link({local, couch_batch_save}, ?MODULE, [BatchSize, BatchInterval], []).

%%--------------------------------------------------------------------
%% Function: commit_doc(Doc) -> committed
%% Description: Puts the doc into the set to commit. Does not reply until
%% the commit is complete.
%%--------------------------------------------------------------------
eventually_save_doc(DbName, Doc, UserCtx) ->
    ok = gen_server:call(couch_batch_save, {eventually_save_doc, DbName, Doc, UserCtx}, infinity).

%%--------------------------------------------------------------------
%% Function: commit_now(DbName) -> committed
%% Description: Commits all docs for the DB. Does not reply until
%% the commit is complete.
%%--------------------------------------------------------------------
commit_now(DbName) ->
    committed = gen_server:call(couch_batch_save, {commit_now, DbName}, infinity).

%%--------------------------------------------------------------------
%% Function: commit_now() -> committed
%% Description: Commits all docs for all DBs. Does not reply until
%% the commit is complete.
%%--------------------------------------------------------------------
commit_now() ->
    committed = gen_server:call(couch_batch_save, commit_now, infinity).
 
    
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init([BatchSize, BatchInterval]) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server with the meanings
%%--------------------------------------------------------------------
init([BatchSize, BatchInterval]) ->
    % start a process that calls commit_now/0 every BatchInterval milliseconds
    ets:new(couch_batch_save_by_db, [duplicate_bag, private, named_table]),
    spawn_link(fun() -> commit_every_ms(BatchInterval) end),
    {ok, #batch_state{batch_size=BatchSize}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({eventually_save_doc, DbName, Doc, UserCtx}, _From, #batch_state{
        batch_size=BatchSize
    }=State) ->
    % add the doc to the set
    true = ets:insert(couch_batch_save_by_db, {{DbName, UserCtx}, Doc}),
    NumDocs = ets:select_count(couch_batch_save_by_db, 
        [{[{{DbName, '_'}, '_'}],[],['$$']}]),

    if NumDocs >= BatchSize ->
        commit_docs(DbName);
    true -> ok
    end,
    {reply, ok, State};

handle_call(commit_now, _From, State) ->
    ok = commit_all_dbs(),
    {reply, committed, State};

handle_call({commit_now, DbName}, _From, State) ->
    ok = commit_docs(DbName),
    {reply, committed, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    % todo shutdown the interval loop
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
commit_docs(DbName) ->
    UserCtxs = user_ctxs(DbName),
    lists:foldl(fun(UserCtx) ->
            UserDocsL = ets:match(couch_batch_save_by_db, 
                {{DbName, UserCtx}, '$1'}),
            Docs = [Doc || [Doc] <- UserDocsL],
            {ok, _Revs} = commit_user_docs(DbName, UserCtx, Docs),
            []
        end, [], UserCtxs),        
    ok.

% commit the docs for all dbs
commit_all_dbs() ->
    DbNamesL = ets:match(couch_batch_save_by_db, {{'$1', '_'}, '_'}),
    [commit_docs(DbName) || [DbName] <- DbNamesL],
    ok.

user_ctxs(DbName) ->
    AllCtxs = ets:match(couch_batch_save_by_db, {{DbName, '$1'}, '_'}),
    [Ctx || [Ctx] <- lists:usort(AllCtxs)].

commit_user_docs(DbName, UserCtx, Docs) ->
    case couch_db:open(DbName, [{user_ctx, UserCtx}]) of
    {ok, Db} ->
        try
            {ok, _Revs} = couch_db:update_docs(Db, Docs)
        after
            couch_db:close(Db)
        end;
    Error ->
        throw(Error)
    end.

% spawned to trigger commits on an interval
commit_every_ms(BatchInterval) ->
    receive
        after BatchInterval ->
            couch_batch_save:commit_now(),
            commit_every_ms(BatchInterval)
    end.
