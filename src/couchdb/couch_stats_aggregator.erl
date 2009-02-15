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

% TODO: 
%  - nicer code
%  - comments

-module(couch_stats_aggregator).
-include("stats.hrl").

-ifdef(TEST).
  -include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-export([start/0, stop/0, 
         get/1, get/2, all/0,
         time_passed/0, clear_aggregates/1]).

-record(state, {
    aggregates = []
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
get(Key, Time) ->
    gen_server:call(?MODULE, {get, Key, Time}).

time_passed() ->
    gen_server:call(?MODULE, time_passed).

clear_aggregates(Time) ->
    gen_server:call(?MODULE, {clear_aggregates, Time}).

all() ->
    gen_server:call(?MODULE, all).

% GEN_SERVER
    
init(_) ->
    ets:new(?MODULE, [named_table, set, protected]),
    init_counter(),
    {ok, #state{}}.

handle_call({get, Key}, _, State) ->
    Value = get_aggregate(Key, State),
    {reply, Value, State};

handle_call({get, Key, Time}, _, State) ->
    Value = get_aggregate(Key, State, Time),
    {reply, Value, State};

handle_call(time_passed, _, OldState) ->

    % the foldls below could probably be refactored into a less code-duping form

    % update aggregates on incremental counters
    NextState = lists:foldl(fun(Counter, State) -> 
        {Key, Value} = Counter,
        update_aggregates_loop(Key, Value, State, incremental)
    end, OldState, ?COLLECTOR:all(incremental)),

    % update aggregates on absolute value counters
    NewState = lists:foldl(fun(Counter, State) -> 
        {Key, Value} = Counter,
        % clear the counter, we've got the important bits in State
        ?COLLECTOR:clear(Key),
        update_aggregates_loop(Key, Value, State, absolute)
    end, NextState, ?COLLECTOR:all(absolute)),

    {reply, ok, NewState};

handle_call({clear_aggregates, Time}, _, State) ->
    {reply, ok, do_clear_aggregates(Time, State)};

handle_call(all, _ , State) ->
    Results = convert(ets:tab2list(?MODULE), State),
    {reply, Results, State};

handle_call(stop, _, State) ->
    {stop, normal, stopped, State}.


% PRIVATE API

%% clear the aggregats record for a specific Time = 60 | 300 | 900
do_clear_aggregates(Time, #state{aggregates=Stats}) ->
    NewStats = lists:foldl(fun({Key, Stat}, _Acc) ->
         case proplists:lookup(Key, Stat) of
            % do have stats for this key, if we don't, return Stats unmodified
            none -> Stat;
            % there are stats, let's unset the Time one
            {Key, Stat} ->
                [{Time, empty} | proplists:delete(Time, Stat)]
        end
    end, 0, Stats),
    #state{aggregates=NewStats}.

%% default Time is 0, which is when CouchDB started
get_aggregate(Key, StatsList) ->
    get_aggregate(Key, StatsList, '0').
get_aggregate(Key, #state{aggregates=StatsList}, Time) ->
    Aggregates = case proplists:lookup(Key, StatsList) of
        % if we don't have any data here, return an empty record
        none -> #aggregates{};
        {Key, Stats} ->
            case proplists:lookup(Time, Stats) of
                none -> #aggregates{}; % empty record again
                {Time, Stat} -> Stat
            end
    end,
    Aggregates.

%% updates all aggregates for Key
update_aggregates_loop(Key, Values, State, CounterType) ->
    #state{aggregates=AllStats} = State,
    % if we don't have any aggregates yet, put a list of empty atoms in
    % so we can loop over them in update_aggregates().
    [{Key, StatsList}] = case AllStats of
        [] -> [{Key, [
                {'0', empty}, 
                {'60', empty},
                {'300', empty},
                {'900', empty}
             ]}];
        _ -> AllStats
    end,

    % if we get called with a single value, wrap in in a list
    ValuesList = case is_list(Values) of
        false -> [Values];
        _True -> Values
    end,
    % loop over all Time's
    NewStats = lists:map(fun({Time, Stats}) ->
        % loop over all values for Key
        lists:foldl(fun(Value, Stat) ->
            {Time, update_aggregates(Value, Stat, CounterType)}
        end, Stats, ValuesList)
    end, StatsList),

    % put the newly calculated aggregates into State and delete the previous
    % entry
    #state{aggregates=[{Key, NewStats} | proplists:delete(Key, AllStats)]}.

% does the actual updating of the aggregate record
update_aggregates(Value, Stat, CounterType) ->
    case Stat of
        % the first time this is called, we don't have to calculate anything
        % we just populate the record with Value
        empty -> #aggregates{
            min=Value,
            max=Value,
            mean=Value,
            variance=0,
            stddev=0,
            count=1,
            last=Value
        };
        % this sure could look nicer -- any ideas?
        StatsRecord ->
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
    end.

% extracts stats for Key from Stats
% bonus points for making this look nicer
get_stats(Key, Stats) ->
    % if we don't have any stats, return an empty record in JSON-erlang-terms 
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

% convert ets2list() list into JSON-erlang-terms.
% Thanks to Paul Davis
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

% Every second, pull counter stats from ?COLLECTOR and aggregte them.
% Every 60, 300, 900 seconds, reset aggregates for the timeframe.
init_counter() ->
    start_timer(1, fun() -> ?MODULE:time_passed() end),
    start_timer(60, fun() -> ?MODULE:clear_aggregates(60) end),
    start_timer(300, fun() -> ?MODULE:clear_aggregates(300) end),
    start_timer(900, fun() -> ?MODULE:clear_aggregates(900) end).

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
