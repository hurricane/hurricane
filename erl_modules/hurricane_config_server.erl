-module(hurricane_config_server).

-export([get_config/1, start/1, start_process/1]).

ensure_externals(Externals, ExternalsTable) ->
    OldPids = ets:match(ExternalsTable, {'$1', '_'}),
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
                    ets:insert(ExternalsTable, {Pid, External}),
                    pg2:join(Name, Pid)
                end,
                lists:seq(1, Num)
            )
        end,
        Externals
    ),
    lists:map(
        fun([OldPid]) ->
            OldPid ! {terminate, erlang:self()}
        end,
        OldPids
    ).

loop(State) ->
    receive
        {_From, reload_config} ->
            ConfigPath = proplists:get_value(config_path, State),
            LoadConfigFun = proplists:get_value(load_config_fun, State),
            StateNoConfig = lists:keydelete(config, 1, State),
            Config = erlang:apply(LoadConfigFun, [ConfigPath]),
            ensure_externals(
                proplists:get_value(externals, Config, []),
                proplists:get_value(externals_table, State)
            ),
            NewState = [{config, Config} | StateNoConfig];
        {From, get_config, Key} ->
            Config = proplists:get_value(config, State, []),
            Value = proplists:get_value(Key, Config),
            From ! {config_for, Key, Value},
            NewState = State;
        _Other ->
            NewState = State
    end,
    loop(NewState).

start_process(State) ->
    ExternalsTable = ets:new(externals_table, [set, private]),
    erlang:process_flag(trap_exit, true),
    NewState = [{externals_table, ExternalsTable} | State],
    loop(NewState).

get_config(Key) ->
    hurricane_config_server ! {erlang:self(), get_config, Key},
    receive
        {config_for, Key, Value} -> ok
    end,
    Value.

start(State) ->
    Pid = erlang:spawn_link(hurricane_config_server, start_process, [State]),
    erlang:register(hurricane_config_server, Pid),
    Pid ! {erlang:self(), reload_config}.
