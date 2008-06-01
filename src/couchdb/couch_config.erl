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

-module(couch_config).
-include("couch_db.hrl").

-define(DEFAULT_INI, "couch.ini").

-behaviour(gen_server).
-export([start_link/0, init/1   , 
    handle_call/3, handle_cast/2, handle_info/2, 
    terminate/2, code_change/3]).
-export([store/2, 
    lookup/1, lookup/2, lookup_match/1, lookup_match/2, dump/0,
    init_value/2, unset/1, load_ini_file/1, 
    load_ini_files/1]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).    

init_value(Key, Value) -> gen_server:call(?MODULE, {init_value, Key, Value}).
store(Key, Value) -> gen_server:call(?MODULE, {store, [{Key, Value}]}).
lookup(Key) -> gen_server:call(?MODULE, {lookup, Key}).
lookup(Key, Default) -> gen_server:call(?MODULE, {lookup, Key, Default}).
lookup_match(Key) -> gen_server:call(?MODULE, {lookup_match, Key}).
lookup_match(Key, Default) -> gen_server:call(?MODULE, {lookup_match, Key, Default}).
dump() -> gen_server:call(?MODULE, {dump, []}).


unset(Key) -> gen_server:call(?MODULE, {unset, Key}).


init([]) ->     
    Tap = ets:new(?MODULE, []),
    {ok, Tap}.


handle_call({store, Config}, _From, Tab) ->
    [ets:insert(Tab, {Key, Value}) || {Key, Value} <- Config],
    commit(Config),
    {reply, ok, Tab};

handle_call({init_value, Key, Value}, _From, Tab) ->
    Reply = ets:insert(Tab, {Key, Value}),
    {reply, Reply, Tab};

handle_call({unset, Key}, _From, Tab) ->
    ets:delete(Tab, Key),
    {reply, ok, Tab};

handle_call({lookup, Key}, _From, Tab) ->
    lookup(Key, fun(Tab_, Key_) -> ets:lookup(Tab_, Key_) end, null, Tab);

handle_call({lookup, Key, Default}, _From, Tab) ->
    lookup(Key, fun(Tab_, Key_) -> ets:lookup(Tab_, Key_) end, Default, Tab);

handle_call({lookup_match, Key}, _From, Tab) ->
    lookup(Key, fun(Tab_, Key_) -> ets:match(Tab_, Key_) end, null, Tab);

handle_call({lookup_match, Key, Default}, _From, Tab) ->
    lookup(Key, fun(Tab_, Key_) -> ets:match(Tab_, Key_) end, Default, Tab);

handle_call({dump, []}, _From, Tab) ->
    io:format("~p~n", [ets:match(Tab, '$1')]),
    {reply, ok, Tab}.



lookup(Key, Fun, Default, Tab) ->
    Reply = case Fun(Tab, Key) of
        [{Key, Value}] ->
            Value;
        [List] ->
            lists:map(fun([Key2, Value2]) -> {Key2, Value2} end, [List]);
        [] ->
            Default
    end,
    {reply, Reply, Tab}.

commit(Config) ->
    couch_config_writer:save_config(Config, ?MODULE:lookup({"_CouchDB", "ConfigurationStorageFile"})),
    ok.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

load_ini_files(IniFiles) ->
    % load all ini files in the order they come in.
    lists:foreach(fun(IniFile) -> load_ini_file(IniFile) end, IniFiles),
    
    % store the name of the last ini file for storing changes made at runtime
    [LastIniFile|_] = lists:reverse(IniFiles),
    couch_config:init_value({"_CouchDB", "ConfigurationStorageFile"}, LastIniFile).


load_ini_file(IniFile) ->
    IniFilename = couch_util:abs_pathname(IniFile),
    IniBin =
    case file:read_file(IniFilename) of
        {ok, IniBin0} ->
           IniBin0;
        {error, enoent} ->
           Msg = io_lib:format("Couldn't find server configuration file ~s.", [IniFilename]),
           io:format("~s~n", [Msg]),
           throw({startup_error, Msg})
    end,
    
    {ok, Lines} = regexp:split(binary_to_list(IniBin), "\r\n|\n|\r|\032"),
    {_, ParsedIniValues} =
    lists:foldl(fun(Line, {AccSectionName, AccValues}) ->
            case string:strip(Line) of
            "[" ++ Rest ->
                case regexp:split(Rest, "\\]") of
                {ok, [NewSectionName, ""]} ->
                    {NewSectionName, AccValues};
                _Else -> % end bracket not at end, ignore this line
                    {AccSectionName, AccValues}
                end;
            ";" ++ _Comment ->
                {AccSectionName, AccValues};
            Line2 ->
                case regexp:split(Line2, "=") of
                {ok, [_SingleElement]} -> % no "=" found, ignore this line
                    {AccSectionName, AccValues};
                {ok, [""|_LineValues]} -> % line begins with "=", ignore
                    {AccSectionName, AccValues};
                {ok, [ValueName|LineValues]} -> % yeehaw, got a line!
                    RemainingLine = couch_util:implode(LineValues, "="),
                    {ok, [LineValue | _Rest]} = regexp:split(RemainingLine, " ;|\t;"), % removes comments
                    {AccSectionName, [{{AccSectionName, ValueName}, LineValue} | AccValues]}
                end
            end
        end, {"", []}, Lines),
        
        lists:foreach(
            fun({Key, Value}) ->
                couch_config:init_value(Key, Value)
            end,
            lists:reverse(ParsedIniValues)
        ),
    ok.
