-module(hurricane_config_server).

-export([get_config/1, start/1, loop/1]).

ensure_externals(Externals) ->
    lists:map(
        fun(External) ->
            Cmd = proplists:get_value(cmd, External),
            Num = proplists:get_value(num, External),
            Name = proplists:get_value(name, External),
            Module = proplists:get_value(module, External, sync),
            pg2:create(Name),
            lists:map(
                fun(_N) ->
                    Pid = erlang:spawn_link(Module, start, [Cmd]),
                    pg2:join(Name, Pid)
                end,
                lists:seq(1, Num)
            )
        end,
        Externals
    ).

loop(State) ->
    receive
        {_From, reload_config} ->
            ConfigPath = proplists:get_value(config_path, State),
            LoadConfigFun = proplists:get_value(load_config_fun, State),
            StateNoConfig = lists:keydelete(config, 1, State),
            Config = erlang:apply(LoadConfigFun, [ConfigPath]),
            ensure_externals(proplists:get_value(externals, Config, [])),
            NewState = [{config, Config} | StateNoConfig];
        {From, get_config, Key} ->
            Config = proplists:get_value(config, State, []),
            Value = proplists:get_value(Key, Config),
            From ! {config_for, Key, Value},
            NewState = State;
        Other ->
            io:format(standard_error, "Unknown Message: ~p~n", [Other]),
            NewState = State
    end,
    loop(NewState).

get_config(Key) ->
    hurricane_config_server ! {erlang:self(), get_config, Key},
    receive
        {config_for, Key, Value} -> ok
    end,
    Value.

start(State) ->
    Pid = erlang:spawn_link(hurricane_config_server, loop, [State]),
    erlang:register(hurricane_config_server, Pid),
    Pid ! {erlang:self(), reload_config}.
