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


-export([start/0, stop/0, get/1, time_passed/1]).

-record(state, {}).

% PUBLIC API

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
stop() ->
    gen_server:call(?MODULE, stop).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

time_passed(Time) ->
    gen_server:call(?MODULE, {time_passed, Time}).


% GEN_SERVER
    
init(_) ->
    ets:new(?MODULE, [named_table, set, protected]),
    ets:insert(?MODULE, {{<<"httpd">>, <<"previous_request_count">>}, {0, 0}}),
    % ets:insert(?MODULE, {<<"average_open_tables">>, 0}),
    start_timer(60, fun() -> ?MODULE:time_passed({minute, 1}) end),
    {ok, #state{}}.

handle_call({get, {Module, Key}}, _, State) ->
    Value = 
    case Key of
        <<"average_",CollectorKey/binary>> ->
            get_average(Module, CollectorKey);
        _ -> 
            couch_stats_collector:get({Module, Key})
    end,
    {reply, integer_to_binary(Value), State};

handle_call(stop, _, State) ->
    {stop, normal, stopped, State};
    
handle_call({time_passed, Time}, _, State) ->
    lists:foreach(fun(Counter) -> 
        {{Module, Key}, {_, PreviousCount}} = Counter,
        CurrentCount = couch_stats_collector:get({Module, get_collector_key(Key)}),
        ets:insert(?MODULE, {{Module, Key}, {PreviousCount, CurrentCount}})
    end, ets:tab2list(?MODULE)),
    {reply, ok, State}.

% PRIVATE API

integer_to_binary(Integer) ->
    list_to_binary(integer_to_list(Integer)).

get_average(Module, Key) ->
    case ets:lookup(?MODULE, {Module, <<"previous_",Key/binary>>}) of
        [] -> 0;
        [{_, {PreviousCounter, CurrentCounter}}] -> 
            round((CurrentCounter - PreviousCounter) / 60)
    end.

get_collector_key(Key) ->
    <<"previous_", CollectorKey/binary>> = Key,
    CollectorKey.

start_timer(Time, Fun) ->
    spawn(fun() -> timer(Time * 1000, Fun) end).

timer(Time, Fun) ->
    receive
        cancel -> void
    after
        Time -> Fun(),
        timer(Time, Fun)
    end.

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

should_return_the_average_over_the_last_minute_test() ->
    catch ?MODULE:stop(),
    ?MODULE:start(),
    couch_stats_collector:start(),

    lists:map(fun(_) ->
        couch_stats_collector:increment({<<"httpd">>, <<"request_count">>})
    end, lists:seq(1, 200)),

    ?MODULE:time_passed({minute, 1}),
    ?assertEqual(<<"3">>, ?MODULE:get({<<"httpd">>, <<"average_request_count">>})),
    
    ?MODULE:stop(),
    couch_stats_collector:stop().
