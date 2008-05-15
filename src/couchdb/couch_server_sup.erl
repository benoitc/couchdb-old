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


-export([start_link/1,stop/0]).

-include("couch_db.hrl").

%% supervisor callbacks
-export([init/1]).

start_link(IniFiles) ->
    case whereis(couch_server_sup) of
    undefined ->
        couch_config:start_link(),
        couch_config:load_ini_files(IniFiles),
        start_server();
    _Else ->
        {error, already_started}
    end.

start_server() ->
    case init:get_argument(pidfile) of
    {ok, [PidFile]} ->
        case file:write_file(PidFile, os:getpid()) of
        ok -> ok;
        Error -> io:format("Failed to write PID file ~s, error: ~p", [PidFile, Error])
        end;
    _ -> ok
    end,

    ConsoleStartupMsg = couch_config:lookup({"CouchDB", "ConsoleStartupMsg"}),
    UpdateNotificationProcesses = couch_config:lookup({"CouchDB", "UpdateNotificationProcesses"}, []),
    FtSearchQueryServer = couch_config:lookup({"Fulltext", "QueryServer"}, []),
    ChildProcesses =
        [{couch_log,
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
        % {couch_util,
        %     {couch_util, start_link, []},
        %     permanent,
        %     brutal_kill,
        %     worker,
        %     [couch_util]},
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
        ] ++
        lists:map(fun(UpdateNotificationProcess) when is_list(UpdateNotificationProcesses) ->
            {UpdateNotificationProcess,
                {couch_db_update_notifier, start_link, [UpdateNotificationProcess]},
                permanent,
                1000,
                supervisor,
                [couch_db_update_notifier]}
            end, UpdateNotificationProcesses)
        ++
        case FtSearchQueryServer of
        "" ->
            [];
        _ ->
            [{couch_ft_query,
                {couch_ft_query, start_link, [FtSearchQueryServer]},
                permanent,
                1000,
                supervisor,
                [couch_ft_query]}]
        end,

    io:format("~s~n~n", [ConsoleStartupMsg]),


    couch_util:start_driver(),

    % ensure these applications are running
    application:start(inets),
    application:start(crypto),

    process_flag(trap_exit, true),
    StartResult = (catch supervisor:start_link(
        {local, couch_server_sup}, couch_server_sup, ChildProcesses)),

    case StartResult of
    {ok,_} ->
        % only output when startup was successful
        io:format("Apache CouchDB has started, time to relax. See http://~s:~s/_utils/index.html~n",
            [couch_config:lookup({"HTTPd", "BindAddress"}), couch_config:lookup({"HTTPd", "Port"})]);
    _ ->
        % Since we failed startup, unconditionally dump configuration data to console
        ok = couch_config:dump()
    end,
    process_flag(trap_exit, false),
    StartResult.

stop() ->
    catch exit(whereis(couch_server_sup), normal),
    couch_config:stop(),
    couch_log:stop().

init(ChildProcesses) ->
    {ok, {{one_for_one, 10, 3600}, ChildProcesses}}.

% dump_config() ->
%     io:format("Debug: ~p~n", [dying]),
%     ?LOG_DEBUG("~s", [okay]).
%     % {ok, Cwd} = file:get_cwd(),
%     % ConfigInfo = io_lib:format("Config Info:~n\tCurrentWorkingDir=~s~n" ++
%     %        "\tDbRootDir=~s~n" ++
%     %        "\tBindAddress=~p~n" ++
%     %        "\tPort=~p~n" ++
%     %        "\tDocumentRoot=~s~n" ++
%     %        "\tLogFile=~s~n" ++
%     %        "\tLogLevel=~s~n" ++
%     %        "\tUtilDriverDir=~s~n" ++
%     %        "\tDbUpdateNotificationProcesses=~s~n" ++
%     %        "\tFullTextSearchQueryServer=~s~n" ++
%     %        "~s",
%     %            [Cwd,
%     %            couch_config:lookup({couchdb, root_directory}),
%     %            couch_config:lookup({couchdb, util_driver_dir}),
%     %            couch_config:lookup({couchdb, update_notification_processes}),
%     %            couch_config:lookup({httpd, bind_address}),
%     %            couch_config:lookup({httpd, port}),
%     %            couch_config:lookup({httpd, document_root}),
%     %            couch_config:lookup({log, file}),
%     %            couch_config:lookup({log, level}),
%     %            couch_config:lookup({fulltext, query_server}),
%     %            [lists:flatten(io_lib:format("\t~s=~s~n", [Lang, QueryExe])) || {Lang, QueryExe} <- couch_config:lookup({couchdb, view_query_servers})]]),
%     %            ?LOG_INFO("~s", [ConfigInfo]).
