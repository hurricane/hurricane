%%% The main hurricane module, responsible for starting root services.

-module(hurricane).

-export([start/1]).

%% Starts basic services, such as configuration, logging, and the
%% supervisor. The supervisor will start all subsequent services, as
%% defined in the config.
start(Options) ->
    application:start(sasl),
    hurricane_config_server:start(Options),
    hurricane_log_server:start([]),
    erlang:spawn_link(hurricane_supervisor, start, [[]]).
