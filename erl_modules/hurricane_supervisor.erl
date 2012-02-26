%%% Manages the lifecycle of most modules in Hurricane. Deals with
%%% starting processes, remembering what config is tied to what Pids,
%%% and re-starting processes when they exit.

-module(hurricane_supervisor).

-export([start/1]).

%% Starts all modules given.
start_modules(StartModules) ->
    lists:map(
        fun(StartModule) ->
            Module = proplists:get_value(module, StartModule),
            Function = proplists:get_value(function, StartModule, start),
            Args = proplists:get_value(args, StartModule, []),
            Count = proplists:get_value(count, StartModule, 1),
            start_module_instances(Module, Function, Args, Count)
        end,
        StartModules
    ).

%% Starts multiple instances of the same module.
start_module_instances(Module, Function, Args, Count) ->
    lists:map(
        fun(_N) ->
            start_module(Module, Function, Args)
        end,
        lists:seq(1, Count)
    ).

%% Starts a single module.
start_module(Module, Function, Args) ->
    hurricane_log_server:log(
        info,
        "supervisor starting module: [~p, ~p, ~p]",
        [Module, Function, [Args]]
    ),
    Pid = erlang:spawn_link(Module, Function, [Args]),
    erlang:put(Pid, {Module, Function, Args}).

%% Runs forever, restarting any processes that exit.
loop() ->
    receive
        {'EXIT', Pid, normal} ->
            hurricane_log_server:log(
                info,
                "~p exited normally, supervisor will not restart.",
                [Pid]
            );
        {'EXIT', Pid, Reason} ->
            hurricane_log_server:log(
                error,
                "~p ~p, supervisor restarting...",
                [Pid, Reason]
            ),
            {Module, Function, Args} = erlang:get(Pid),
            erlang:erase(Pid),
            start_module(Module, Function, Args);
        Other ->
            hurricane_log_server:log(
                error,
                "supervisor received unknown message: ~p~n",
                [Other]
            )
    end,
    loop().

%% Registers itself under a convenience name. Starts all modules and
%% enters the service loop.
start(_Args) ->
    erlang:process_flag(trap_exit, true),
    erlang:register(?MODULE, erlang:self()),
    StartModules = hurricane_config_server:get_config(start_modules, []),
    start_modules(StartModules),
    loop().
