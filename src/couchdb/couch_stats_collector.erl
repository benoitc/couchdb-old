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

-module(couch_stats_collector).

-define(TEST, true).
-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).


-export([start/0, stop/0, get/1, increment/1, decrement/1]).

-record(state, {}).


% PUBLIC API

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
stop() ->
    gen_server:call(?MODULE, stop).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

increment(Key) ->
    gen_server:call(?MODULE, {increment, Key}).

decrement(Key) ->
    gen_server:call(?MODULE, {decrement, Key}).

% GEN_SERVER
    
init(_) ->
    ets:new(?MODULE, [named_table, set, protected]),
    {ok, #state{}}.

handle_call({get, Key}, _, State) ->
    Result = case ets:lookup(?MODULE, Key) of
        [] -> 0;
        [{_,Result1}] -> Result1
    end,
    {reply, Result, State};
    
handle_call({increment, Key}, _, State) ->
    case catch ets:update_counter(?MODULE, Key, 1) of
        {'EXIT', {badarg, _}} -> ets:insert(?MODULE, {Key, 1});
        _ -> ok
    end,
    {reply, ok, State};
    
handle_call({decrement, Key}, _, State) ->
    case catch ets:update_counter(?MODULE, Key, -1) of
        {'EXIT', {badarg, _}} -> ets:insert(?MODULE, {Key, -1});
        _ -> ok
    end,
    {reply, ok, State};
    
% handle_call(reset, _, State) ->
%     ets:insert(?MODULE, {Key, 0}),
%     {reply, ok, 0};
    
handle_call(stop, _, State) ->
    {stop, normal, stopped, State}.


% Unused gen_server behaviour API functions that we need to declare.
  
%% @doc Unused
handle_cast(foo, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Unused
terminate(_Reason, _State) -> ok.

%% @doc Unused
code_change(_OldVersion, State, _Extra) -> {ok, State}.

% TESTS  

test_helper(Fun) ->
    catch ?MODULE:stop(),
    ?MODULE:start(),

    Fun(),

    ?MODULE:stop().

should_return_value_from_store_test() ->
    test_helper(fun() -> 
        ?assertEqual(0, ?MODULE:get({couch_db, open_databases}))
    end).

should_increment_value_test() ->
    test_helper(fun() ->
        ?assert(?MODULE:increment({couch_db, open_databases}) =:= ok),
        ?assertEqual(1, ?MODULE:get({couch_db, open_databases}))
    end).

should_decrement_value_test() ->
    test_helper(fun() ->
        ?assert(?MODULE:decrement({couch_db, open_databases}) =:= ok),
        ?assertEqual(-1, ?MODULE:get({couch_db, open_databases}))
    end).

should_increment_and_decrement_value_test() ->
    test_helper(fun() ->
        ?assert(?MODULE:increment({couch_db, open_databases}) =:= ok),
        ?assert(?MODULE:decrement({couch_db, open_databases}) =:= ok),
        ?assertEqual(0, ?MODULE:get({couch_db, open_databases}))
    end).

should_reset_counter_value_test() ->
    test_helper(fun() ->
        ?assert(?MODULE:increment({couch_db, open_databases}) =:= ok),
        ?MODULE:stop(),
        ?MODULE:start(),
        ?assertEqual(0, ?MODULE:get({couch_db, open_databases}))
    end).

should_handle_multiple_key_value_pairs_test() ->
    test_helper(fun() ->
        ?MODULE:increment({couch_db, open_databases}),
        ?assertEqual(1, ?MODULE:get({couch_db, open_databases})),
        ?assertEqual(0, ?MODULE:get({couch_db, request_count}))
    end).

should_restart_module_should_create_new_pid_test() ->
    test_helper(fun() ->
        OldPid = whereis(?MODULE),
        ?MODULE:stop(),
        ?MODULE:start(),
        ?assertNot(whereis(?MODULE) =:= OldPid)
    end).
