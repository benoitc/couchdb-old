-module(couch_rpc).

-behaviour(gen_server).

-export([start_link/0, server_info/0, db_info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    loop}).
    

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

    
handle_call(server_info, From, State) ->
    ServerInfo = [
        {couchdb, <<"Welcome">>},
        {version, list_to_binary(couch_server:get_version())}
    ],
    {reply, ServerInfo, State};

handle_call({db_info, DbName}, From, State) ->
    {ok, Db} = couch_db:open(DbName, [?ADMIN_USER_CTX]),
    R = couch_db:get_db_info(Db),
    {reply, R, State}.
     
handle_cast(_Msg, State) ->
    {noreply, State}.    
    
handle_info(_Info, State) ->
    {noreply, State}.
    
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
terminate(_Reason, _State) ->
    ok.
    
