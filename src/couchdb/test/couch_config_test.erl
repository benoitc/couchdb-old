% couch_config module test suote

% Set up test suite
% ?MODULE_test() returns a list of functions 
% that run the actual tests.
couch_config_test() ->
    [
        fun() -> store_tuples() end, 
        fun() -> store_strings() end,
        fun() -> store_numbers() end,
        fun() -> store_tuple_key() end
    ].


% test functions

% test storing different types and see if they come back
% the same way there put in.
store_tuples() ->
    basic_store(key, value).
  
store_strings() ->
    basic_store("key", "value").

store_numbers() ->
    basic_store("number_key", 12345).

store_tuple_key() ->
    basic_store({key, subkey}, value).

    
basic_store(Key, Value) ->
    couch_config:start_link(),

    couch_config:init_value(Key, Value),
    Result = couch_config:lookup(Key),
    couch_config:unset(Key),

    couch_config:stop(),
    Value = Result.