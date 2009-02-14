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

-export([start/0, stop/0, get/1, time_passed/0, all/0]).

-record(state, {
    aggregates = []
}).

-record(aggregates, {
    min=0,
    max=0,
    mean=0.0,
    variance = 0.0,
    stddev=0.0,
    count=0,
    last=0
}).

-define(COLLECTOR, couch_stats_collector).
-define(QUEUE_MAX_LENGTH, 900). % maximimum number of seconds

% PUBLIC API

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
stop() ->
    gen_server:call(?MODULE, stop).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

time_passed() ->
    gen_server:call(?MODULE, time_passed).

all() ->
    gen_server:call(?MODULE, all).

% GEN_SERVER
    
init(_) ->
    ets:new(?MODULE, [named_table, set, protected]),
    init_counter(),
    {ok, #state{}}.

handle_call({get, {Module, Key}}, _, State) ->
    Value = get_aggregate({Module, Key}, State),
    {reply, Value, State};

% update all counters that match `Time` = int()
handle_call(time_passed, _, OldState) ->
    % minmax
    NextState = lists:foldl(fun(Counter, State) -> 
        {Key, Value} = Counter,
        update_aggregates_loop(Key, Value, State, incremental)
    end, OldState, ?COLLECTOR:all(incremental)),

    NewState = lists:foldl(fun(Counter, State) -> 
        {Key, Value} = Counter,
        update_aggregates_loop(Key, Value, State, absolute)
    end, NextState, ?COLLECTOR:all(absolute)),

    {reply, ok, NewState};

handle_call(all, _ , State) ->
    Results = convert(?COLLECTOR:all(), State),
    {reply, Results, State};

handle_call(stop, _, State) ->
    {stop, normal, stopped, State}.


% PRIVATE API

get_aggregate(Key, #state{aggregates=Stats}) ->
    Aggregates = case proplists:lookup(Key, Stats) of
        none -> #aggregates{};
        {Key, Other} -> Other
    end,
    Aggregates.

update_aggregates_loop(Key, Values, OldState, CounterType) when is_list(Values) ->
    lists:foldl(fun(Value, State) ->
        update_aggregates(Key, Value, State, CounterType)
    end, OldState, Values);
update_aggregates_loop(Key, Value, State, CounterType) ->
    update_aggregates(Key, Value, State, CounterType).

update_aggregates(Key, Value, #state{aggregates=Stats}, CounterType) ->
    % ?debugFmt("Value: '~p'~n", [Value]),
    % ?debugFmt("OldStats: '~p'~n", [Stats]),
    NewStats = case proplists:lookup(Key, Stats) of
        none -> #aggregates{
            min=Value,
            max=Value,
            mean=Value,
            variance=0,
            stddev=0,
            count=1,
            last=Value
        };
        {_Key, StatsRecord} ->
            #aggregates{
                min=Min,
                max=Max,
                mean=Mean,
                variance=Variance,
                count=Count,
                last=Last
            } = StatsRecord,

            % incremental counters need to keep track of the last update's value
            NewValue = case CounterType of
                incremental -> Value - Last;
                absolute -> Value
            end,
            % Knuth, The Art of Computer Programming, vol. 2, p. 232. 
            NewCount = Count + 1,
            NewMean = Mean + (NewValue - Mean) / NewCount, % NewCount is never 0.
            NewVariance = Variance + (NewValue - Mean) * (NewValue - NewMean),
            #aggregates{
                min=lists:min([NewValue, Min]),
                max=lists:max([NewValue, Max]),
                mean=NewMean,
                variance=NewVariance,
                stddev=math:sqrt(NewVariance / NewCount),
                count=NewCount,
                last=Value
            }
    end,
    % ?debugFmt("'~p'~n", [NewStats]),
    #state{aggregates=[{Key, NewStats} | proplists:delete(Key, Stats)]}.

get_stats(Key, Stats) ->
    case proplists:lookup(Key, Stats) of
        none -> {[
            {current, 0},
            {mean, 0},
            {min, 0},
            {max, 0},
            {stddev, 0},
            {resolution, 1}
        ]};
    {_Key, StatsRecord} ->
        #aggregates{
            min=Min,
            max=Max,
            mean=Mean,
            stddev=Stddev,
            count=Count
        } = StatsRecord,
        {[
            {current, Count},
            {mean, Mean},
            {min, Min},
            {max, Max},
            {stddev, Stddev},
            {resolution, 1}
        ]}
    end.

convert(In, Stats) ->
    [{LastMod, LastVals} | LastRestMods] = lists:foldl(fun({{Module, Key}, _Count}, AccIn) ->
        case AccIn of
            [] ->
                [{Module, [{Key, get_stats(Key, Stats)}]}];
            [{Module, PrevVals} | RestMods] ->
                [{Module, [{Key, get_stats(Key, Stats)} | PrevVals]} | RestMods];
            [{OtherMod, ModVals} | RestMods] ->
                [{Module, [{Key, get_stats(Key, Stats)}]}, {OtherMod, {lists:reverse(ModVals)}} | RestMods]
        end
    end, [], lists:sort(In)),
    {[{LastMod, {lists:sort(LastVals)}} | LastRestMods]}.


% TIMER

init_counter() ->
    start_timer(1, fun() -> ?MODULE:time_passed() end). % fire every second

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

test_helper(Fun) ->
    catch ?MODULE:stop(),
    ?MODULE:start(),
    ?COLLECTOR:start(),

    Fun(),

    ?MODULE:stop(),
    ?COLLECTOR:stop().

should_return_value_from_collector_test() ->
    test_helper(fun() ->
        #aggregates{count=Result} = ?MODULE:get({couch_db, open_databases}),
        ?assertEqual(0, Result)
    end).

should_handle_multiple_key_value_pairs_test() ->
    test_helper(fun() ->
        ?COLLECTOR:increment({couch_db, open_databases}),
        ?MODULE:time_passed(),
        #aggregates{count=OpenDbs} = ?MODULE:get({couch_db, open_databases}),
        ?assertEqual(1, OpenDbs),
        #aggregates{count=RequestCount} = ?MODULE:get({couch_db, request_count}),
        ?assertEqual(0, RequestCount)
    end).

should_return_the_mean_over_the_last_minute_test() ->
    test_helper(fun() ->
        lists:map(fun(_) ->
            ?COLLECTOR:increment({httpd, request_count}),
            ?COLLECTOR:increment({httpd, request_count}),
            ?COLLECTOR:increment({httpd, request_count}),
            ?MODULE:time_passed() % one second passed
        end, lists:seq(1, 2)),
        #aggregates{mean=Mean} = ?MODULE:get({httpd, request_count}),
        ?assert(Mean - 3.00 < 0.1)
    end).
    
should_return_the_stddev_value_per_second_test() ->
    test_helper(fun() ->
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?MODULE:time_passed(), % one second passed
        ?COLLECTOR:increment({httpd, request_count}),
        ?MODULE:time_passed(), % one second passed

        #aggregates{stddev=Stddev} = ?MODULE:get({httpd, request_count}),
        ?assertEqual(1.0, Stddev)
    end).

should_return_min_aggregate_counter_test() ->
    test_helper(fun() -> 
        ?COLLECTOR:record({couchdb, request_time}, 20),
        ?COLLECTOR:record({couchdb, request_time}, 30),
        ?MODULE:time_passed(),
        #aggregates{min=Min} = ?MODULE:get({couchdb, request_time}),
        ?assertEqual(20, Min)
    end).

should_return_max_aggregate_counter_test() ->
    test_helper(fun() -> 
        ?COLLECTOR:record({couchdb, request_time}, 20),
        ?COLLECTOR:record({couchdb, request_time}, 30),
        ?MODULE:time_passed(),
        #aggregates{max=Max} = ?MODULE:get({couchdb, request_time}),
        ?assertEqual(30, Max)
    end).
