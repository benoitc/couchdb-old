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


-export([start/0, stop/0, get/1, get/2, time_passed/1]).

-record(state, {}).

% PUBLIC API

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
stop() ->
    gen_server:call(?MODULE, stop).

get(Key) ->
    get(Key, []).
get(Key, Options) ->
    gen_server:call(?MODULE, {get, Key, Options}).

time_passed(Time) ->
    gen_server:call(?MODULE, {time_passed, Time}).


% GEN_SERVER
    
init(_) ->
    ets:new(?MODULE, [named_table, set, protected]),
    lists:map(fun(Time) -> init_counter(Time) end, [1, 5, 15]),
    {ok, #state{}}.

handle_call({get, {Module, Key}, Options}, _, State) ->
    Value = 
    case Key of
        <<"average_",CollectorKey/binary>> ->
            get_average(Module, CollectorKey, Options);
        _ -> 
            couch_stats_collector:get({Module, Key})
    end,
    
    {reply, integer_to_binary(Value), State};

handle_call(stop, _, State) ->
    {stop, normal, stopped, State};

% update all counters that match `Time` = int()
handle_call({time_passed, Time}, _, State) ->
    lists:foreach(fun(Counter) ->
        {{Module, Key, _}, {_, PreviousCount}} = Counter,
        CurrentCount = couch_stats_collector:get({Module, get_collector_key(Key)}),
        ets:insert(?MODULE, {{Module, Key, Time}, {PreviousCount, CurrentCount}})
    end, ets:tab2list(?MODULE)),
    {reply, ok, State}.

% PRIVATE API

get_average_counters() ->
    [{<<"httpd">>, <<"previous_request_count">>}].

get_average(Module, Key, Options) ->
    Time = proplists:get_value("timeframe", Options, 1) * 60, % default to 1 minute, in seconds
    case ets:lookup(?MODULE, {Module, <<"previous_",Key/binary>>, Time}) of
        [] -> 0;
        [{_, {PreviousCounter, CurrentCounter}}] ->
            round((CurrentCounter - PreviousCounter) / Time)
    end.

get_collector_key(Key) ->
    <<"previous_", CollectorKey/binary>> = Key,
    CollectorKey.

init_counter(Time) ->
    Seconds = Time * 60,
    lists:map(
        fun({Module, Key}) ->
            start_timer(Seconds, fun() -> ?MODULE:time_passed(Seconds) end),
            ets:insert(?MODULE, {{Module, Key, Time}, {0, 0}})
        end, get_average_counters()).

integer_to_binary(Integer) ->
    list_to_binary(integer_to_list(Integer)).

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
    couch_stats_collector:start(),

    Fun(),

    ?MODULE:stop(),
    couch_stats_collector:stop().

should_return_value_from_collector_test() ->
    test_helper(fun() ->
        ?assertEqual(<<"0">>, couch_stats_aggregator:get({<<"couch_db">>, <<"open_databases">>}))
    end).

should_handle_multiple_key_value_pairs_test() ->
    test_helper(fun() ->
        couch_stats_collector:increment({<<"couch_db">>, <<"open_databases">>}),
        ?assertEqual(<<"1">>, couch_stats_aggregator:get({<<"couch_db">>, <<"open_databases">>})),
        ?assertEqual(<<"0">>, couch_stats_aggregator:get({<<"couch_db">>, <<"request_count">>}))
    end).

should_return_the_average_over_the_last_minute_test() ->
    test_helper(fun() ->
        lists:map(fun(_) ->
            couch_stats_collector:increment({<<"httpd">>, <<"request_count">>})
        end, lists:seq(1, 200)),

        ?MODULE:time_passed(60), % seconds
        ?assertEqual(<<"3">>, ?MODULE:get({<<"httpd">>, <<"average_request_count">>}))
    end).

should_return_the_average_over_the_last_five_minutes_test() ->
    test_helper(fun() ->
        lists:map(fun(_) ->
            couch_stats_collector:increment({<<"httpd">>, <<"request_count">>})
        end, lists:seq(1, 2000)),

        ?MODULE:time_passed(300), % seconds
        Result = ?MODULE:get({<<"httpd">>, <<"average_request_count">>}, [{"timeframe", list_to_integer("5")}]),
        ?assertEqual(<<"7">>, Result)
    end).
