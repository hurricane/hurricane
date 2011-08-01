#!/usr/bin/env escript
%%! -smp enable -sname hurricane

main(Args) ->
    LoadConfigFun = fun(ConfigPath) ->
        {ok, Config} = file:consult(ConfigPath),
        Modules = proplists:get_value(modules, Config, []),
        lists:map(
            fun(Filepath) ->
                {ok, ModuleName} = compile:file(Filepath),
                {ok, ModuleName, Binary} = compile:file(
                    Filepath, [binary]
                ),
                {module, ModuleName} = code:load_binary(
                    ModuleName, Filepath, Binary
                )
            end,
            Modules
        ),
        code:add_pathsz(proplists:get_value(add_code_paths, Config, [])),
        Config
    end,
    case erlang:length(Args) of
        0 ->
            io:format(standard_error, "No config file path given.~n", []),
            erlang:exit(erlang:self());
        _ ->
            ok
    end,

    ConfigPath = lists:nth(1, Args),
    erlang:apply(LoadConfigFun, [ConfigPath]),
    hurricane:start(
        [{config_path, ConfigPath}, {load_config_fun, LoadConfigFun}]
    ),
    block().

block() ->
    receive
        _ -> ok
    end,
    block().
