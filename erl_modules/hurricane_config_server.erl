%%% Starts and registers the config server. The config server can be
%%% used by any process that wants to access the config (convenience
%%% functions are provided for this).

-module(hurricane_config_server).

-export([get_config/1, start/1]).

%% Runs forever. Supports reloading the config and serving config
%% values up to any process that asks for them.
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

%% Provides a convenient way for other processes to get the config from
%% the config server (encapsulates the messaging that has to happen and
%% how to receive a reply).
get_config(Key) ->
    ?MODULE ! {erlang:self(), get_config, Key},
    receive
        {config_for, Key, Value} -> ok
    end,
    Value.

%% Registers the process under a convenience name, send a message to
%% load the config, and enters the serve loop.
start(State) ->
    erlang:register(?MODULE, erlang:self()),
    erlang:self() ! {erlang:self(), reload_config},
    loop(State).
