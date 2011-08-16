-module(hurricane_config_server).

-export([get_config/1, start/1]).

loop(State) ->
    receive
        {_From, reload_config} ->
            ConfigPath = proplists:get_value(config_path, State),
            LoadConfigFun = proplists:get_value(load_config_fun, State),
            StateNoConfig = lists:keydelete(config, 1, State),
            Config = erlang:apply(LoadConfigFun, [ConfigPath]),
            NewState = [{config, Config} | StateNoConfig];
        {From, get_config, Key} ->
            Config = proplists:get_value(config, State, []),
            Value = proplists:get_value(Key, Config),
            From ! {config_for, Key, Value},
            NewState = State;
        Other ->
            io:format(
                standard_error,
                "config server received unknown message ~p~n",
                [Other]
            ),
            NewState = State
    end,
    loop(NewState).

get_config(Key) ->
    ?MODULE ! {erlang:self(), get_config, Key},
    receive
        {config_for, Key, Value} -> ok
    end,
    Value.

start(State) ->
    erlang:register(?MODULE, erlang:self()),
    erlang:self() ! {erlang:self(), reload_config},
    loop(State).
