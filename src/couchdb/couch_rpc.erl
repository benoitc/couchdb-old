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

-module(couch_rpc).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([server_info/0, db_info/1, create_db/1, delete_db/1, 
         save_doc/2, open_doc/2]).

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
    CouchDoc = couch_httpd_db:couch_doc_open(Db, DocId, nil, []),
    Doc = couch_doc:to_json_obj(CouchDoc, []),
    {reply, {ok, Doc}, State}.
     
handle_cast(_Msg, State) ->
    {noreply, State}.    
    
handle_info(_Info, State) ->
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
terminate(_Reason, _State) ->
    ok.
    

%% Internal Api
open_db(DbName) ->
    couch_db:open(DbName, [?ADMIN_USER_CTX]).
    
ejson_to_couch_doc({DocProps}) ->
    Doc = case proplists:get_value(<<"_id">>, DocProps) of
        undefined ->
            DocId = couch_util:new_uuid(),
            {[{<<"_id">>, DocId}|DocProps]};
        DocId ->
            {DocProps}
    end,
    couch_doc:from_json_obj(Doc).