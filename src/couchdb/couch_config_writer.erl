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

-module(couch_config_writer).
-export([save_config/2]).

save_config({{Module, Variable}, Value}, File) ->
    % open file and create a list of lines
    {ok, Stream} = file:read_file(File),
    {ok, Lines} = regexp:split(binary_to_list(Stream), "\r\n|\n|\r|\032"),
    
    % prepare input variables
    ModuleName = "[" ++ atom_to_list(Module) ++ "]",
    VariableList = atom_to_list(Variable),
    
    % produce the contents for the config file
    NewFileContents = save_loop({{ModuleName, VariableList}, Value}, Lines, "", "", []),
    
    % do the save, close the config file and get out
    save_file(File, NewFileContents),
    file:close(Stream),
    ok.
    
save_loop({{Module, Variable}, Value}, [Line|Rest], OldCurrentModule, Contents, DoneVariables) ->

    % if we find a new [ini section] (Module), save that for reference
    NewCurrentModule = parse_module(Line, OldCurrentModule),

    % if the current Module is the one we want to change, try to match
    % each line with the Variable
    NewContents = case Module of
        NewCurrentModule ->
            % see if the current line matches the variable we want to substitute
            case parse_variable(Line, Variable, Value) of
                % nope, return original line
                nomatch ->
                    DoneVariables2 = DoneVariables,
                    Line;
                % got em! return new line
                NewLine ->
                    DoneVariables2 = [Variable|DoneVariables],
                    NewLine
            end;
        % if the variable we want to change couldn't be replaced, we append it
        % in the proper module section
        OldCurrentModule ->
            case lists:member(Variable, DoneVariables) of
                false ->
                    DoneVariables2 = [Variable|DoneVariables],
                    Variable ++ "=" ++ Value ++ "\n" ++ Line;
                true ->
                    DoneVariables2 = DoneVariables,
                    Line
            end;
        % otherwise we just print out the original line
        _ ->
            DoneVariables2 = DoneVariables,
            Line
        end,
    % clumsy way to only append a newline character
    % if the line is not empty. We need this to not
    % avoid haveing a newline inserted at the top
    % of the target file each time we save it.
    Contents2 = case Contents of "" -> ""; _ -> Contents ++ "\n" end,

    % go to next line
    save_loop({{Module, Variable}, Value}, Rest, NewCurrentModule, Contents2 ++ NewContents, DoneVariables2);
    
save_loop(_Config, [], _OldModule, NewFileContents, _DoneVariable) ->
    % we're out of new lines, just return the new file's contents
    NewFileContents.
        
parse_module(Line, OldModule) ->
    case regexp:match(Line, "^\\[([a-zA-Z0-9_-]*)\\]$") of
        nomatch ->
            OldModule;
        {error, Error} ->
            io:format("ini file regex error module: '~s'~n", [Error]),
            OldModule;
        {match, Start, Length} ->
            string:substr(Line, Start, Length)
    end.

parse_variable(Line, Variable, Value) ->
    case regexp:match(Line, "^" ++ Variable ++ "=") of
        nomatch ->
            nomatch;
        {error, Error}->
            io:format("ini file regex error variable: '~s'~n", [Error]),
            nomatch;
        {match, _Start, _Length} ->
            Variable ++ "=" ++ Value
    end.

save_file(File, Contents) ->
    file:write_file(File, list_to_binary(Contents)).