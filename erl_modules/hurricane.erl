-module(hurricane).

-export([start/1]).

start(Options) ->
    application:start(sasl),
    erlang:spawn_link(hurricane_config_server, start, [Options]),
    erlang:spawn_link(hurricane_log_server, start, [[]]),
    erlang:spawn_link(hurricane_supervisor, start, [[]]).
