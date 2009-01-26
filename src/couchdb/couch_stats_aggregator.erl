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


-export([start/0, get/1]).

-record(state, {}).

% PUBLIC API

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
stop() ->
    gen_server:call(?MODULE, stop).


get({Module, Key}) ->
  gen_server:call(?MODULE, {get}).
  

% GEN_SERVER
    
init(_) ->
    {ok, #state{}}.

handle_call({get}, _, State) ->
  {reply, <<"1">>, State}.

% Unused gen_server behaviour API functions that we need to declare.
  
%% @doc Unused
handle_cast(foo, State) -> {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Unused
terminate(_Reason, _State) -> ok.

%% @doc Unused
code_change(_OldVersion, State, _Extra) -> {ok, State}.

% TESTS  
should_return_value_from_collector_test()->
  couch_stats_aggregator:start(),
  try ?assert(<<"1">> =:= couch_stats_aggregator:get({<<"couch_db">>, <<"open_databases">>}))
  catch
    Exp -> throw(Exp)
  after
    couch_stats_aggregator:terminate(test_end, ok)
  end.