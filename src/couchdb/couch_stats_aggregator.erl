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


-export([start/0, stop/0, get/1, get/2, time_passed/0]).

-record(state, {}).

-define(COLLECTOR, couch_stats_collector).
-define(QUEUE_MAX_LENGTH, 900). % maximimum number of seconds

% PUBLIC API

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
stop() ->
    gen_server:call(?MODULE, stop).

get(Key) ->
    get(Key, []).
get(Key, Options) ->
    gen_server:call(?MODULE, {get, Key, Options}).

time_passed() ->
    gen_server:call(?MODULE, time_passed).


% GEN_SERVER
    
init(_) ->
    ets:new(?MODULE, [named_table, set, protected]),
    init_counter(),
    {ok, #state{}}.

handle_call({get, {ModuleBinary, Key}, Options}, _, State) ->
    Module = b2a(ModuleBinary),
    Value = 
    case a2b(Key) of
        <<"average_",CollectorKey/binary>> ->
            queue_extract_average(get_queue({Module, b2a(CollectorKey)}, Options));
        <<"max_",CollectorKey/binary>> ->
            queue_extract_max(get_queue({Module, b2a(CollectorKey)}, Options));
        <<"min_",CollectorKey/binary>> ->
            queue_extract_min(get_queue({Module, b2a(CollectorKey)}, Options));
        <<"stddev_",CollectorKey/binary>> ->
            queue_extract_stddev(get_queue({Module, b2a(CollectorKey)}, Options));
        _ -> 
            ?COLLECTOR:get({Module, b2a(Key)})
    end,
    
    {reply, integer_to_binary(Value), State};

handle_call(stop, _, State) ->
    {stop, normal, stopped, State};

% update all counters that match `Time` = int()
handle_call(time_passed, _, State) ->
    % minmax
    lists:foreach(fun(Counter) -> 
        {Key, Count} = Counter,
        Queue = maybe_initialise_queue(Key),
        queue_append(Queue, Key, Count) % drops after QUEUE_MAX_LENGTH
    end, ?COLLECTOR:all()),
    {reply, ok, State}.

% PRIVATE API

maybe_initialise_queue(Key) -> 
    case ets:lookup(?MODULE, Key) of
        [] -> queue:new();
        [{Key, Queue}] -> Queue
    end.

queue_append(Queue, Key, Value) ->
    NewQueue = queue_truncate(queue:in(Value, Queue)),
    ets:insert(?MODULE, {Key, NewQueue}).

queue_truncate(Queue) ->
    case queue:len(Queue) > ?QUEUE_MAX_LENGTH of
        true -> 
            {_Head, TruncatedQueue} = queue:out(Queue),
            TruncatedQueue;
        false -> Queue
    end.

get_queue(Key, Options) ->
    Time = proplists:get_value("timeframe", Options, 60) + 1, % we need one more element to determine the differences between elements
    [{_, Queue}] = ets:lookup(?MODULE, Key),
    SplitLength = lists:min([Time, queue:len(Queue)]),
    {QueueInTime, _} = queue:split(SplitLength, queue:reverse(Queue)),
    queue:reverse(QueueInTime).

queue_extract_average(Queue) ->
    Differences = queue_to_differences_list(Queue),
    round(list_average(Differences)).

list_average(List) ->
    lists:sum(List) / length(List).

queue_extract_max(Queue) ->
    lists:max(queue_to_differences_list(Queue)).
    
queue_extract_min(Queue) ->
    lists:min(queue_to_differences_list(Queue)).

queue_extract_stddev(Queue) ->
    Differences = queue_to_differences_list(Queue),
    Average = list_average(Differences),
    Deviations = lists:map(fun(Elem) -> 
        abs(Elem - Average)
    end, Differences),
    round(list_average(Deviations)).

queue_to_differences_list(Queue) ->
    [Head|List] = queue:to_list(Queue),
    {_Prev, Result} = lists:foldl(fun(Elm, {Prev, AccList}) ->
        {Elm, [Elm - Prev|AccList]}
    end, {Head, []}, List),
    Result.

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

% UTILS

integer_to_binary(Integer) ->
    list_to_binary(integer_to_list(Integer)).

b2a(Binary) when is_atom(Binary)->
    Binary;
b2a(Binary) ->
    list_to_atom(binary_to_list(Binary)).

a2b(Atom) when is_binary(Atom) ->
    Atom;
a2b(Atom) ->
    list_to_binary(atom_to_list(Atom)).

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
        ?assertEqual(<<"0">>, ?MODULE:get({couch_db, open_databases}))
    end).

should_handle_multiple_key_value_pairs_test() ->
    test_helper(fun() ->
        ?COLLECTOR:increment({couch_db, open_databases}),
        ?assertEqual(<<"1">>, ?MODULE:get({couch_db, open_databases})),
        ?assertEqual(<<"0">>, ?MODULE:get({couch_db, request_count}))
    end).

should_return_the_average_over_the_last_minute_test() ->
    test_helper(fun() ->
        lists:map(fun(_) ->
            ?COLLECTOR:increment({httpd, request_count}),
            ?COLLECTOR:increment({httpd, request_count}),
            ?COLLECTOR:increment({httpd, request_count}),
            ?MODULE:time_passed()
        end, lists:seq(1, 60)),
        ?assertEqual(<<"3">>, ?MODULE:get({httpd, average_request_count}))
    end).

should_return_the_max_value_per_second_over_time_test() ->
    test_helper(fun() ->
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?MODULE:time_passed(), % seconds
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?MODULE:time_passed(), % seconds

        Result = ?MODULE:get({httpd, max_request_count}, [{"timeframe", 60}]),
        ?assertEqual(<<"3">>, Result)
    end).

should_return_the_min_value_per_second_over_time_test() ->
    test_helper(fun() ->
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?MODULE:time_passed(), % seconds
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?MODULE:time_passed(), % seconds
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?MODULE:time_passed(), % seconds
        Result = ?MODULE:get({httpd, min_request_count}, [{"timeframe", 60}]),
        ?assertEqual(<<"2">>, Result)
    end).

should_return_the_max_value_per_second_over_the_given_time_period_only_test() ->
    test_helper(fun() ->
        ?COLLECTOR:increment({httpd, request_count}),
        ?MODULE:time_passed(), % seconds
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?MODULE:time_passed(), % seconds
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?MODULE:time_passed(), % seconds

        Result = ?MODULE:get({httpd, max_request_count}, [{"timeframe", 1}]),
        ?assertEqual(<<"2">>, Result)
    end).
    
should_return_the_stddev_value_per_second_test() ->
    test_helper(fun() ->
        ?COLLECTOR:increment({httpd, request_count}),
        ?MODULE:time_passed(), % seconds
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?MODULE:time_passed(), % seconds
        ?COLLECTOR:increment({httpd, request_count}),
        ?COLLECTOR:increment({httpd, request_count}),
        ?MODULE:time_passed(), % seconds

        Result = ?MODULE:get({httpd, stddev_request_count}),
        ?assertEqual(<<"1">>, Result)
    end).

should_not_sample_more_than_QUEUE_MAX_LENGTH_seconds_test() ->
    test_helper(fun() ->
        lists:map(fun(_) ->
            ?COLLECTOR:increment({httpd, request_count}),
            ?COLLECTOR:increment({httpd, request_count}),
            ?COLLECTOR:increment({httpd, request_count}),
            ?COLLECTOR:increment({httpd, request_count}),
            ?COLLECTOR:increment({httpd, request_count}),
            ?COLLECTOR:increment({httpd, request_count}),
            ?COLLECTOR:increment({httpd, request_count}),
            ?COLLECTOR:increment({httpd, request_count}),
            ?COLLECTOR:increment({httpd, request_count}),
            ?COLLECTOR:increment({httpd, request_count}),
            ?COLLECTOR:increment({httpd, request_count}),
            ?MODULE:time_passed()
        end, lists:seq(1, 100)),
        lists:map(fun(_) ->
            ?COLLECTOR:increment({httpd, request_count}),
            ?MODULE:time_passed()
        end, lists:seq(1, 900)),

        Result = ?MODULE:get({httpd, average_request_count}, [{"timeframe", 1000}]),
        ?assertEqual(<<"1">>, Result)
    end).