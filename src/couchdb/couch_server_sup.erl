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

-module(couch_server_sup).
-behaviour(supervisor).


-export([start_link/1,stop/0,couch_config_start_link_wrapper/2]).

-include("couch_db.hrl").

%% supervisor callbacks
-export([init/1]).

start_link(IniFiles) ->
    case whereis(couch_server_sup) of
    undefined ->
        start_server(IniFiles);
    _Else ->
        {error, already_started}
    end.

couch_config_start_link_wrapper(IniFiles, FirstConfigPid) ->
    case is_process_alive(FirstConfigPid) of
        true ->
            link(FirstConfigPid),
            {ok, FirstConfigPid};
        false -> couch_config:start_link(IniFiles)
    end.

start_server(IniFiles) ->
    case init:get_argument(pidfile) of
    {ok, [PidFile]} ->
        case file:write_file(PidFile, os:getpid()) of
        ok -> ok;
        Error -> io:format("Failed to write PID file ~s, error: ~p", [PidFile, Error])
        end;
    _ -> ok
    end,
    {ok, ConfigPid} = couch_config:start_link(IniFiles),
    
    LibDir =
    case couch_config:get({"CouchDB", "UtilDriverDir"}, null) of
    null ->
        filename:join(code:priv_dir(couch), "lib");
    LibDir0 -> LibDir0
    end,
    
    ok = couch_util:start_driver(LibDir),

    
    ChildProcesses =
        [{couch_config,
            {couch_server_sup, couch_config_start_link_wrapper, [IniFiles, ConfigPid]},
            permanent,
            brutal_kill,
            worker,
            [couch_config]},
        {couch_log,
            {couch_log, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_server]},
        {couch_db_update_event,
            {gen_event, start_link, [{local, couch_db_update}]},
            permanent,
            1000,
            supervisor,
            dynamic},
        {couch_server,
            {couch_server, sup_start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_server]},
        {couch_query_servers,
            {couch_query_servers, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_query_servers]},
        {couch_view,
            {couch_view, start_link, []},
            permanent,
            brutal_kill,
            worker,
            [couch_view]},
        {couch_httpd,
            {couch_httpd, start_link, []},
            permanent,
            1000,
            supervisor,
            [couch_httpd]}
        ],
   

    % ensure these applications are running
    application:start(inets),
    application:start(crypto),

    {ok, Pid} = supervisor:start_link(
        {local, couch_server_sup}, couch_server_sup, ChildProcesses),
    io:format("started"),
    % launch the icu bridge
    % just restart if one of the config settings change.

    couch_config:register(
        fun({"CouchDB", "UtilDriverDir"}) ->
            ?MODULE:stop()
        end, Pid),
    
    % we only get where when startup was successful
    BindAddress = couch_config:get({"HTTPd", "BindAddress"}),
    Port = couch_config:get({"HTTPd", "Port"}),
    io:format("Apache CouchDB has started, see http://~s:~s/_utils/index.html~n",
            [BindAddress, Port]),
    {ok, Pid}.


stop() ->
    catch exit(whereis(couch_server_sup), normal).

init(ChildProcesses) ->
    {ok, {{one_for_one, 10, 3600}, ChildProcesses}}.
