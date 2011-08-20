%%% The main hurricane module, responsible for starting root services.

-module(hurricane).

-export([start/1]).

%% Starts basic services, such as configuration, logging, and the
%% supervisor. The supervisor will start all subsequent services, as
%% defined in the config.
start(Options) ->
    application:start(sasl),
    erlang:spawn_link(hurricane_config_server, start, [Options]),
    erlang:spawn_link(hurricane_log_server, start, [[]]),
    erlang:spawn_link(hurricane_supervisor, start, [[]]).
