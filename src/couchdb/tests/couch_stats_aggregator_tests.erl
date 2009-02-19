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

-module(couch_stats_aggregator_tests).
-include_lib("eunit/include/eunit.hrl").

-include("../couch_stats.hrl").

test_helper(Fun) ->
    catch couch_stats_aggregator:stop(),
    couch_stats_aggregator:start(),
    couch_stats_collector:start(),

    Fun(),

    couch_stats_aggregator:stop(),
    couch_stats_collector:stop().

should_return_value_from_collector_test() ->
    test_helper(fun() ->
        #aggregates{count=Result} = couch_stats_aggregator:get({couch_db, open_databases}),
        ?assertEqual(0, Result)
    end).

should_handle_multiple_increment_counter_key_value_pairs_test() ->
    test_helper(fun() ->
        couch_stats_collector:increment({couch_db, open_databases}),
        couch_stats_aggregator:time_passed(),
        #aggregates{count=OpenDbs} = couch_stats_aggregator:get({couch_db, open_databases}),
        ?assertEqual(1, OpenDbs),
        #aggregates{count=RequestCount} = couch_stats_aggregator:get({couch_db, request_count}),
        ?assertEqual(0, RequestCount)
    end).

should_handle_multiple_absolute_counter_key_value_pairs_test() ->
    test_helper(fun() ->
        couch_stats_collector:record({httpd, request_time}, 20),
        couch_stats_collector:record({httpd, request_size}, 40),
        couch_stats_aggregator:time_passed(),
        
        #aggregates{max=RequestTime} = couch_stats_aggregator:get({httpd, request_time}),
        ?assertEqual(20, RequestTime),
        #aggregates{max=RequestSize} = couch_stats_aggregator:get({httpd, request_size}),
        ?assertEqual(40, RequestSize)
    end).

should_return_the_mean_over_the_last_minute_test() ->
    test_helper(fun() ->
        lists:map(fun(_) ->
            couch_stats_collector:increment({httpd, request_count}),
            couch_stats_collector:increment({httpd, request_count}),
            couch_stats_collector:increment({httpd, request_count}),
            couch_stats_aggregator:time_passed() % one second passed
        end, lists:seq(1, 2)),
        #aggregates{mean=Mean} = couch_stats_aggregator:get({httpd, request_count}),
        ?assert(Mean - 3.00 < 0.1)
    end).

should_return_the_stddev_value_per_second_test() ->
    test_helper(fun() ->
        couch_stats_collector:increment({httpd, request_count}),
        couch_stats_collector:increment({httpd, request_count}),
        couch_stats_collector:increment({httpd, request_count}),
        couch_stats_aggregator:time_passed(), % one second passed
        couch_stats_collector:increment({httpd, request_count}),
        couch_stats_aggregator:time_passed(), % one second passed

        #aggregates{stddev=Stddev} = couch_stats_aggregator:get({httpd, request_count}),
        ?assertEqual(1.0, Stddev)
    end).

should_return_min_aggregate_counter_test() ->
    test_helper(fun() -> 
        couch_stats_collector:record({couchdb, request_time}, 20),
        couch_stats_collector:record({couchdb, request_time}, 30),
        couch_stats_aggregator:time_passed(),
        #aggregates{min=Min} = couch_stats_aggregator:get({couchdb, request_time}),
        ?assertEqual(20, Min)
    end).

should_return_max_aggregate_counter_test() ->
    test_helper(fun() -> 
        couch_stats_collector:record({couchdb, request_time}, 20),
        couch_stats_collector:record({couchdb, request_time}, 30),
        couch_stats_aggregator:time_passed(),
        #aggregates{max=Max} = couch_stats_aggregator:get({couchdb, request_time}),
        ?assertEqual(30, Max)
    end).

should_return_aggregate_value_for_timerange_test() ->
    test_helper(fun() -> 
        couch_stats_collector:record({couchdb, request_time}, 20),
        couch_stats_collector:record({couchdb, request_time}, 30),
        couch_stats_aggregator:time_passed(),
        #aggregates{max=Max} = couch_stats_aggregator:get({couchdb, request_time}, '300'),
        ?assertEqual(30, Max)
    end).

should_clear_aggregates_on_timeout_test() ->
    test_helper(fun() -> 
        couch_stats_collector:record({couchdb, request_time}, 20),
        couch_stats_collector:record({couchdb, request_time}, 30),
        couch_stats_aggregator:time_passed(),
        couch_stats_aggregator:clear_aggregates('60'),
        #aggregates{max=Max} = couch_stats_aggregator:get({couchdb, request_time}, '60'),
        ?assertEqual(0, Max)
    end).

should_clear_aggregates_on_timeout_but_not_for_other_timeouts_test() ->
    test_helper(fun() -> 
        couch_stats_collector:record({couchdb, request_time}, 20),
        couch_stats_collector:record({couchdb, request_time}, 30),
        couch_stats_aggregator:time_passed(),
        couch_stats_aggregator:clear_aggregates('60'),
        #aggregates{max=Max} = couch_stats_aggregator:get({couchdb, request_time}, '300'),
        ?assertEqual(30, Max)
    end).
