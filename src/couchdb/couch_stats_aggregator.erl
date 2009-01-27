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

-module(couch_stats_aggregator).

-define(TEST, true).
-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).


-export([start/0, stop/0, get/1]).

-record(state, {}).

% PUBLIC API

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
stop() ->
    gen_server:call(?MODULE, stop).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).


% GEN_SERVER
    
init(_) ->
    {ok, #state{}}.

handle_call({get, Key}, _, State) ->
    Value = integer_to_binary(couch_stats_collector:get(Key)),
    {reply, Value, State};
handle_call(stop, _, State) ->
    {stop, normal, stopped, State}.

% PRIVATE API

integer_to_binary(Integer) ->
    list_to_binary(integer_to_list(Integer)).


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
should_return_value_from_collector_test() ->
    catch ?MODULE:stop(),
    ?MODULE:start(),
    couch_stats_collector:start(),
    ?assertEqual(<<"0">>, couch_stats_aggregator:get({<<"couch_db">>, <<"open_databases">>})),
    ?MODULE:stop(),
    couch_stats_collector:stop().

should_handle_multiple_key_value_pairs_test() ->
    catch ?MODULE:stop(),
    ?MODULE:start(),
    couch_stats_collector:start(),
    
    couch_stats_collector:increment({<<"couch_db">>, <<"open_databases">>}),
    ?assertEqual(<<"1">>, couch_stats_aggregator:get({<<"couch_db">>, <<"open_databases">>})),
    ?assertEqual(<<"0">>, couch_stats_aggregator:get({<<"couch_db">>, <<"request_count">>})),
    
    ?MODULE:stop(),
    couch_stats_collector:stop().

    