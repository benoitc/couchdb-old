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

%% @doc Reads CouchDB's ini file and gets queried for configuration parameters.
%%      This module is initialized with a list of ini files that it 
%%      consecutively reads Key/Value pairs from and saves them in an ets 
%%      table. If more an one ini file is specified, the last one is used to 
%%      write changes that are made with store/2 back to that ini file.
%% @author Jan Lehnardt <jan@apache.org>

-module(couch_config).
-include("couch_db.hrl").

-behaviour(gen_server).
-export([start_link/0, init/1, stop/0,
    handle_call/3, handle_cast/2, handle_info/2, 
    terminate/2, code_change/3]).
-export([store/2, register/2, 
    lookup/1, lookup/2,
    lookup_match/1, lookup_match/2, 
    lookup_and_register/2, lookup_and_register/3,
    lookup_match_and_register/2, lookup_match_and_register/3,
    dump/0, init_value/2, unset/1, load_ini_file/1, 
    load_ini_files/1]).

-define(COUCH_CONFIG_CALLBACK, "_CouchDBConfigChangeCallback").
%% Public API %%

%% @type etstable() = integer().

%% @spec start_link() -> {ok, Tab::etsatable()}
%% @doc Start the configuration module
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).    

%% @spec stop() -> ok
%% @doc Stops the configuration module
stop() ->
    ok.

%% @spec init_value(Key::any(), Value::any()) -> {ok, Tab::etsatable()}
%% @doc Public API function triggers initialization of a Key/Value pair. Used 
%%      when setting values from the ini file. Works like store/2 but doesn't
%%      write the Key/Value pair to the storage ini file.
init_value(Key, Value) -> gen_server:call(?MODULE, {init_value, Key, Value}).

%% @spec store(Key::any(), Value::any()) -> {ok, Tab::etsatable()}
%% @doc Public API function that triggers storage of a Key/Value pair into the
%%      local ets table and writes it to the storage ini file.
store(Key, Value) -> gen_server:call(?MODULE, {store, [{Key, Value}]}).

%% @spec lookup(Key::any()) -> Value::any() | null
%% @doc Returns the value that is stored under key::any() or null::atom() if no
%%      such Key exists.
lookup(Key) -> gen_server:call(?MODULE, {lookup, Key}).

%% @spec lookup(Key::any(), Default::any()) -> Value::any() | Default
%% @doc Returns the value that is stored under key::any() or Default::any() if
%%      no such Key exists.
lookup(Key, Default) -> gen_server:call(?MODULE, {lookup, Key, Default}).

%% @spec lookup_and_register(Key::any(), CallbackFunction::function()) ->
%%         Value::any() | null
%% @doc Returns the value that is stored under Key::any() or null::atom() if no
%%      such Key exists. Additionally, this functions registers 
%%      CallbackFunction::function() to be called if the value of Key::any()
%%      is changed at a later point.
lookup_and_register(Key, CallbackFunction) -> 
    gen_server:call(?MODULE, {lookup_and_register, Key, CallbackFunction}).

%% @spec lookup_and_register(
%%         Key::any(),
%%         Default::any(),
%%         CallbackFunction::function()) -> Value::any() | Default
%% @doc Returns the value that is stored under Key::any() or Default::any() if
%%      such Key exists. Additionally, this functions registers 
%%      CallbackFunction::function() to be called if the value of Key::any()
%%      is changed at a later point.
lookup_and_register(Key, Default, CallbackFunction) ->
    gen_server:call(?MODULE, {lookup_and_register, Key, Default, CallbackFunction}).

%% @spec lookup_match(Key::any()) -> Value::any() | null:atom()
%% @doc Lets you look for a Key's Value specifying a pattern that gets passed 
%%      to ets::match(). Returns null::atom() if no Key is found.
lookup_match(Key) -> gen_server:call(?MODULE, {lookup_match, Key}).

%% @spec lookup_match(Key::any(), Default::any()) -> Value::any() | Default
%% @doc Lets you look for a Key's Value specifying a pattern that gets passed 
%%      to ets::match(). Returns Default::any() if no Key is found
lookup_match(Key, Default) -> gen_server:call(?MODULE, {lookup_match, Key, Default}).

%% @spec lookup_match_and_register(Key::any(), CallbackFunction::function()) ->
%%           Value::any() | null:atom()
%% @doc Lets you look for a Key's Value specifying a pattern that gets passed 
%%      to ets::match(). Returns null::atom() if no Key is found. Additionally,
%%      this functions registers CallbackFunction::function() to be called if 
%%      the value of Key::any() is changed at a later point.
lookup_match_and_register(Key, CallbackFunction) ->
    gen_server:call(?MODULE, {lookup_match_and_register, Key, CallbackFunction}).

%% @spec lookup_match_and_register(
%%           Key::any(), Default::any(), CallbackFunction::function()) ->
%%               Value::any() | Default
%% @doc Lets you look for a Key's Value specifying a pattern that gets passed 
%%      to ets::match(). Returns null::atom() if no Key is found. Additionally,
%%      this functions registers CallbackFunction::function() to be called if 
%%      the value of Key::any() is changed at a later point.
lookup_match_and_register(Key, Default, CallbackFunction) ->
    gen_server:call(?MODULE, {lookup_match_and_register, Key, Default, CallbackFunction}).

%% @spec dump() -> ok:atom()
%% @doc Dumps the current ets table with all configuration data.
dump() -> gen_server:call(?MODULE, {dump, []}).

%% @spec register(Key::any(), Fun::function()) -> ok
%% @doc Public API function that registers a function to be called when the
%%      Value of Key::any() is changed.
register(Key, Fun) -> gen_server:call(?MODULE, {register, Key, Fun}).

%% @spec unset(Key::any) -> ok
%% @doc Public API call to remove the configuration entry from the internal 
%%      ets table. This change is _not_ written to the storage ini file.
unset(Key) -> gen_server:call(?MODULE, {unset, Key}).

%% Private API %%

%% @spec init(List::list([])) -> {ok, Tab::etsatable()}
%% @doc Creates a new ets table of the type "set".
init([]) ->     
    Tab = ets:new(?MODULE, [set]),
    {ok, Tab}.

%% @doc see store/2
handle_call({store, Config}, _From, Tab) ->
    [insert_and_commit(Tab, Config2) || Config2 <- Config],
    {reply, ok, Tab};


%% @doc See init_value/2
handle_call({init_value, Key, Value}, _From, Tab) ->
    Reply = ets:insert(Tab, {Key, Value}),
    {reply, Reply, Tab};

%% @doc See unset/1
handle_call({unset, Key}, _From, Tab) ->
    ets:delete(Tab, Key),
    {reply, ok, Tab};


%% @doc See lookup/1
handle_call({lookup, Key}, _From, Tab) ->
    lookup(Key, fun(Tab_, Key_) -> ets:lookup(Tab_, Key_) end, null, Tab);

%% @doc See lookup/2
handle_call({lookup, Key, Default}, _From, Tab) ->
    lookup(Key, fun(Tab_, Key_) -> ets:lookup(Tab_, Key_) end, Default, Tab);

%% @doc See lookup_and_register/2
handle_call({lookup_and_register, Key, CallbackFunction}, _From, Tab) ->
    ?MODULE:handle_call({register, Key, CallbackFunction}, _From, Tab),
    lookup(Key, fun(Tab_, Key_) -> ets:lookup(Tab_, Key_) end, null, Tab);

%% @doc See lookup_and_register/3
handle_call({lookup_and_register, Key, Default, CallbackFunction}, _From, Tab) ->
    ?MODULE:handle_call({register, Key, CallbackFunction}, _From, Tab),
    lookup(Key, fun(Tab_, Key_) -> ets:lookup(Tab_, Key_) end, Default, Tab);

%% @doc See lookup_match/1
handle_call({lookup_match, Key}, _From, Tab) ->
    lookup(Key, fun(Tab_, Key_) -> ets:match(Tab_, Key_) end, null, Tab);

%% @doc See lookup_match/2
handle_call({lookup_match, Key, Default}, _From, Tab) ->
    lookup(Key, fun(Tab_, Key_) -> ets:match(Tab_, Key_) end, Default, Tab);

%% @doc See lookup_match_and_register/2
handle_call({lookup_match_and_register, Key = {{Module, '$1'}, '$2'}, CallbackFunction}, _From, Tab) ->
    ?MODULE:handle_call({register, {Module, ""}, CallbackFunction}, _From, Tab),
    lookup(Key, fun(Tab_, Key_) -> ets:match(Tab_, Key_) end, null, Tab);

%% @doc See lookup_match_and_register/3
handle_call({lookup_match_and_register, Key = {{Module, '$1'}, '$2'}, Default, CallbackFunction}, _From, Tab) ->
    ?MODULE:handle_call({register, {Module, ""}, CallbackFunction}, _From, Tab),
    lookup(Key, fun(Tab_, Key_) -> ets:match(Tab_, Key_) end, Default, Tab);

%% @doc See dump/0
handle_call({dump, []}, _From, Tab) ->
    Config = lists:sort(ets:match(Tab, '$1')),
    lists:foreach(fun([{{Module, Variable}, Value}]) ->
        case Module of
            ?COUCH_CONFIG_CALLBACK ->
                ok; % ignore
            _ ->
                io:format("[~s] ~s=~p~n", [Module, Variable, Value])
        end
    end, Config),
    {reply, ok, Tab};

%% @doc See register/2
handle_call({register, Key, Fun}, From, Tab) ->
    ets:insert(Tab, {{?COUCH_CONFIG_CALLBACK, Key}, {From, Fun}}),
    {reply, ok, Tab}.

%% @spec notify_registered_modules(
%%           Tab::etstable(),
%%           {{Module::string(), Variable::string()}, _Value::any()}) -> ok
%% @doc Looks if a callback function exsists for the specified Module/Variable
%%      combination and calls it.
notify_registered_modules(Tab, {{Module, Variable}, _Value}) ->
    % look for processes that registered for notifications for
    % specific configuration variables
    case ets:lookup(Tab, {?COUCH_CONFIG_CALLBACK, {Module, Variable}}) of
        % found
        [{{?COUCH_CONFIG_CALLBACK, {Module, Variable}}, {_From, Fun}}] ->
            Fun();
        _ -> % not found
            ok
    end,
    
    % look for processes that registerd for notifications for
    % entire modules. Their "Variable" value will be the empty string
     case ets:lookup(Tab, {?COUCH_CONFIG_CALLBACK, {Module, ""}}) of
        % found
        [{{?COUCH_CONFIG_CALLBACK, {Module, _Variable}}, {_From2, Fun2}}] ->
            Fun2();
        _ -> % not found
            ok
    end.

%% @spec lookup(Key::any(), Fun::function(), Default::any(), Tab::etstable()) ->
%%           {reply, Reply::any(), Tab::etstable()}
%% @doc Uses Fun to find the Value for Key. Returns Default if no Value can
%%      be found. Fun is one of ets:lookup/2 and ets:match/2
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

%% @spec insert_and_commit(Tab::etstable(), Config::any()) -> ok
%% @doc Inserts a Key/Value pair into the ets table, writes it to the storage 
%%      ini file and calls all registered callback functions for Key.
insert_and_commit(Tab, Config) ->
    ets:insert(Tab, Config),
    {reply, File, _Tab} = 
        lookup({"_CouchDB", "ConfigurationStorageFile"}, 
            fun(Tab_, Key_) -> ets:lookup(Tab_, Key_) end, 
            null, Tab
        ),

    notify_registered_modules(Tab, Config),
    commit(Config, File).

%% @spec commit(Config::any(), File::filename()) -> ok
%% @doc Writes a Key/Value pair to the ini storage file.
commit(Config, File) ->
    couch_config_writer:save_config(Config, File).

%% @spec load_ini_files([File::filename()]) -> ok
%% @doc Stores the last ini file in Files to be the storage ini file and
%%      iterates over all ini files and calls load_ini_file/1 with each.
load_ini_files(IniFiles) ->
    % store the name of the last ini file for storing changes made at runtime
    [LastIniFile|_] = lists:reverse(IniFiles),
    ?MODULE:init_value({"_CouchDB", "ConfigurationStorageFile"}, LastIniFile),

    % load all ini files in the order they come in.
    lists:foreach(fun(IniFile) -> load_ini_file(IniFile) end, IniFiles).

%% @spec load_ini_file(IniFile::filename()) -> ok
%% @doc Parses an ini file and stores Key/Value Pairs into the ets table.
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
                ?MODULE:init_value(Key, Value)
            end,
            lists:reverse(ParsedIniValues)
        ),
    ok.

% Unused gen_server behaviour API functions that we need to declare.

%% @doc Unused
handle_cast(_Msg, State) -> {noreply, State}.

%% @doc Unused
handle_info(_Msg, State) -> {noreply, State}.

%% @doc Unused
terminate(_Reason, _State) -> ok.

%% @doc Unused
code_change(_OldVersion, State, _Extra) -> {ok, State}.