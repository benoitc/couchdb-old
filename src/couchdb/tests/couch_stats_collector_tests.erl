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
 
-module(couch_stats_collector_tests).

-include_lib("eunit/include/eunit.hrl").

test_helper(Fun) ->
    catch couch_stats_collector:stop(),
    couch_stats_collector:start(),

    Fun(),

    couch_stats_collector:stop().

should_return_value_from_store_test() ->
    test_helper(fun() -> 
        ?assertEqual(0, couch_stats_collector:get({couch_db, open_databases}))
    end).

should_increment_value_test() ->
    test_helper(fun() ->
        couch_stats_collector:increment({couch_db, open_databases}),
        ?assertEqual(1, couch_stats_collector:get({couch_db, open_databases}))
    end).

should_decrement_value_test() ->
    test_helper(fun() ->
        couch_stats_collector:decrement({couch_db, open_databases}),
        ?assertEqual(-1, couch_stats_collector:get({couch_db, open_databases}))
    end).

should_increment_and_decrement_value_test() ->
    test_helper(fun() ->
        couch_stats_collector:increment({couch_db, open_databases}),
        couch_stats_collector:decrement({couch_db, open_databases}),
        ?assertEqual(0, couch_stats_collector:get({couch_db, open_databases}))
    end).

should_reset_counter_value_test() ->
    test_helper(fun() ->
        couch_stats_collector:increment({couch_db, open_databases}),
        couch_stats_collector:stop(),
        couch_stats_collector:start(),
        ?assertEqual(0, couch_stats_collector:get({couch_db, open_databases}))
    end).

should_handle_multiple_key_value_pairs_test() ->
    test_helper(fun() ->
        couch_stats_collector:increment({couch_db, open_databases}),
        ?assertEqual(1, couch_stats_collector:get({couch_db, open_databases})),
        ?assertEqual(0, couch_stats_collector:get({couch_db, request_count}))
    end).

should_restart_module_should_create_new_pid_test() ->
    test_helper(fun() ->
        OldPid = whereis(couch_stats_collector),
        couch_stats_collector:stop(),
        couch_stats_collector:start(),
        ?assertNot(whereis(couch_stats_collector) =:= OldPid)
    end).

should_store_absoulte_value_counter_test() ->
    test_helper(fun() ->
        % record hit
        couch_stats_collector:record({couchdb, request_time}, 20),

        % see if it got into the counter
        List = couch_stats_collector:all(),
        ?assertEqual(1, length(List))
    end).

should_store_absoulte_value_counter_two_hits_test() ->
    test_helper(fun() ->
        % record hit
        couch_stats_collector:record({couchdb, request_time}, 20),
        couch_stats_collector:record({couchdb, request_time}, 30),

        % see if it got into the counter
        List = couch_stats_collector:all(),
        ?assertEqual(2, length(List))
    end).

should_store_absolute_values_for_different_keys_test() ->
    test_helper(fun() ->
        % record hit
        couch_stats_collector:record({couchdb, request_time}, 20),
        couch_stats_collector:record({couchdb, request_time}, 20),
        couch_stats_collector:record({couchdb, request_size}, 30),

        % see if it got into the counter
        Time = couch_stats_collector:get({couchdb, request_time}),
        ?assertEqual([20, 20], Time),

        Size = couch_stats_collector:get({couchdb, request_size}),
        ?assertEqual([30], Size)
    end).

should_clear_absolute_values_for_a_key_test() ->
    test_helper(fun() ->
        % record hit
        couch_stats_collector:record({couchdb, request_time}, 20),
        couch_stats_collector:clear({couchdb, request_time}),
        List = couch_stats_collector:all(),
        ?assertEqual(0, length(List))
    end).

should_clear_absolute_values_for_a_key_but_not_for_other_key_test() ->
    test_helper(fun() ->
        % record hit
        couch_stats_collector:record({couchdb, request_time}, 20),
        couch_stats_collector:record({couchdb, request_size}, 30),
        couch_stats_collector:clear({couchdb, request_time}),
        List = couch_stats_collector:all(),
        ?assertEqual(1, length(List))
    end).
    