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
%%      consecutively reads Key/Value pairs from and saves them in a ets table. 
%%      If more an one ini file is specified, the last one is used to write 
%%      changes that are made with store/2 back to that ini file.
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

%% @spec start_link() -> {ok, Tab}
%% @doc Start the configuration module
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).    

%% @spec stop() -> ok
%% @doc Stops the configuration module
stop() ->
    ok.

%% @spec init_value(Key::any(), Value::any()) -> {ok, Tab}
%% @doc Public API function triggers initialization of a Key/Value pair. Used 
%%      when setting values from the ini file. Works like store/2 but doesn't
%%      write the Key/Value pair to the storage ini file.
init_value(Key, Value) -> gen_server:call(?MODULE, {init_value, Key, Value}).

%% @spec store(Key::any(), Value::any()) -> {ok, Tab}
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

lookup_match_and_register(Key, CallbackFunction) ->
    gen_server:call(?MODULE, {lookup_match_and_register, Key, CallbackFunction}).

lookup_match_and_register(Key, Default, CallbackFunction) ->
    gen_server:call(?MODULE, {lookup_match_and_register, Key, Default, CallbackFunction}).

dump() -> gen_server:call(?MODULE, {dump, []}).
register(Key, Fun) -> gen_server:call(?MODULE, {register, Key, Fun}).

unset(Key) -> gen_server:call(?MODULE, {unset, Key}).

init([]) ->     
    Tab = ets:new(?MODULE, [set]),
    {ok, Tab}.

handle_call({store, Config}, _From, Tab) ->
    [insert_and_commit(Tab, Config2) || Config2 <- Config],
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

handle_call({lookup_and_register, Key, CallbackFunction}, _From, Tab) ->
    ?MODULE:handle_call({register, Key, CallbackFunction}, _From, Tab),
    lookup(Key, fun(Tab_, Key_) -> ets:lookup(Tab_, Key_) end, null, Tab);

handle_call({lookup_and_register, Key, Default, CallbackFunction}, _From, Tab) ->
    ?MODULE:handle_call({register, Key, CallbackFunction}, _From, Tab),
    lookup(Key, fun(Tab_, Key_) -> ets:lookup(Tab_, Key_) end, Default, Tab);
    
handle_call({lookup_match, Key}, _From, Tab) ->
    lookup(Key, fun(Tab_, Key_) -> ets:match(Tab_, Key_) end, null, Tab);

handle_call({lookup_match, Key, Default}, _From, Tab) ->
    lookup(Key, fun(Tab_, Key_) -> ets:match(Tab_, Key_) end, Default, Tab);

handle_call({lookup_match_and_register, Key = {{Module, '$1'}, '$2'}, CallbackFunction}, _From, Tab) ->
    ?MODULE:handle_call({register, {Module, ""}, CallbackFunction}, _From, Tab),
    lookup(Key, fun(Tab_, Key_) -> ets:match(Tab_, Key_) end, null, Tab);

handle_call({lookup_match_and_register, Key = {{Module, '$1'}, '$2'}, Default, CallbackFunction}, _From, Tab) ->
    ?MODULE:handle_call({register, {Module, ""}, CallbackFunction}, _From, Tab),
    lookup(Key, fun(Tab_, Key_) -> ets:match(Tab_, Key_) end, Default, Tab);

handle_call({dump, []}, _From, Tab) ->
    io:format("~p~n", [ets:match(Tab, '$1')]),
    {reply, ok, Tab};
    
handle_call({register, Key, Fun}, From, Tab) ->
    % io:format("registered for '~p' '", [Key]),
    ets:insert(Tab, {{"_CouchDB", Key}, {From, Fun}}),
    {reply, ok, Tab}.

notify_registered_modules(Tab, {{Module, Variable}, _Value}) ->
    % look for processes that registered for notifications for
    % specific configuration variables
    case ets:lookup(Tab, {"_CouchDB", {Module, Variable}}) of
        % found
        [{{"_CouchDB", {Module, Variable}}, {_From, Fun}}] ->
            % io:format("got it '~p'~n", [ets:lookup(Tab, {Module, Variable})]),
            Fun();
        _ -> % not found
            % io:format("not it~n", []),
            ok
    end,
    
    % look for processes that registerd for notifications for
    % entire modules. Their "Variable" value will be the empty string
     case ets:lookup(Tab, {"_CouchDB", {Module, ""}}) of
        % found
        [{{"_CouchDB", {Module, _Variable}}, {_From2, Fun2}}] ->
            Fun2();
        _ -> % not found
            ok
    end.
    

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

insert_and_commit(Tab, Config) ->
    ets:insert(Tab, Config),
    % {Key, _Value} = Config,
    % io:format("just inserted '~p' and now it is '~p~n", [Config, ets:lookup(Tab, Key)]),
    {reply, File, _Tab} = 
        lookup({"_CouchDB", "ConfigurationStorageFile"}, 
            fun(Tab_, Key_) -> ets:lookup(Tab_, Key_) end, 
            null, Tab
        ),

    % io:format("got it '~p' but stored '~p'~n", [ets:lookup(Tab, {"HTTPd", "Port"}), Config]),

    notify_registered_modules(Tab, Config),
    commit(Config, File).

commit(Config, File) ->
    couch_config_writer:save_config(Config, File).

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

load_ini_files(IniFiles) ->
    % store the name of the last ini file for storing changes made at runtime
    [LastIniFile|_] = lists:reverse(IniFiles),
    ?MODULE:init_value({"_CouchDB", "ConfigurationStorageFile"}, LastIniFile),

    % load all ini files in the order they come in.
    lists:foreach(fun(IniFile) -> load_ini_file(IniFile) end, IniFiles).

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
