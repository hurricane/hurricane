#!/usr/bin/env escript
%%! -smp enable -sname hurricane

%% Main entry point into Hurricane. Given a single argument, which is
%% the path to a config file, loads the config, compiles all modules as
%% described in the config, adds all load paths as described in the
%% config, and starts Hurricane.
main(Args) ->
    LoadConfigFun = fun(ConfigPath) ->
        {ok, Config} = file:consult(ConfigPath),
        Config
    end,

    case erlang:length(Args) of
        0 -> erlang:exit(no_config_file_path_given);
        _ -> ok
    end,

    ConfigPath = lists:nth(1, Args),
    Config = erlang:apply(LoadConfigFun, [ConfigPath]),

    Modules = proplists:get_value(compile_modules, Config, []),
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

    hurricane:start(
        [{config_path, ConfigPath}, {load_config_fun, LoadConfigFun}]
    ),
    block().

%% Waits forever. Ensures that the calling process never exits. Useful
%% in this case to keep Hurricane running forever instead of instantly
%% exiting.
block() ->
    receive
        _ -> ok
    end,
    block().
