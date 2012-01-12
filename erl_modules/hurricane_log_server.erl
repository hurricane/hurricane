%%% Handles logging of all events in the system. Exposes a service and
%%% convenience functions.

-module(hurricane_log_server).

-export([start/1, log/2, log/3]).

-define(
    LOG_LEVELS,
    [emergency, alert, critical, error, warning, notice, info, debug]
).

%% Formats the current time in the format YYYY:mm:dd HH:MM:SS.
format_local_time() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    io_lib:format(
        "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
        [Year, Month, Day, Hour, Minute, Second]
    ).

%% Convenience function to log an already-formatted message.
log(Level, Message) ->
    ?MODULE ! {log, format_local_time(), Level, Message}.

%% Convenience function to log a message that needs to be formatted
%% (formatting is handled by io_lib).
log(Level, FormatStr, FormatArgs) ->
    ?MODULE ! {log, format_local_time(), Level, FormatStr, FormatArgs}.

%% Gets all log levels that are loggable (the log levels as defined in
%% this module are sorted, so this function scans until it hits the
%% last turned-on level).
get_loggable_levels(ConfigLevel) ->
    [ConfigLevel | lists:takewhile(
        fun(Level) ->
            Level /= ConfigLevel
        end,
        ?LOG_LEVELS
    )].

%% Takes a list of loggable levels and the log level provided and sees
%% if the level provided can be logged.
is_loggable(LoggableLevels, MessageLevel) ->
    RemainingLevels = lists:filter(
        fun(Level) ->
            Level == MessageLevel
        end,
        LoggableLevels
    ),
    erlang:length(RemainingLevels) > 0.

%% The service loop runs forever with its defined loggable levels. As
%% messages are received, they are inspected for loggability and
%% possibly logged.
loop(LoggableLevels) ->
    receive
        {log, Timestamp, Level, FormatStr, FormatArgs} ->
            case is_loggable(LoggableLevels, Level) of
                true ->
                    Message = io_lib:format(FormatStr, FormatArgs),
                    io:format(
                        standard_error,
                        "[~s] [~s] ~s~n",
                        [Timestamp, Level, Message]
                    );
                _ ->
                    ok
            end;
        Other ->
            io:format(
                standard_error,
                "log server got unexpected message: ~p~n",
                [Other]
            )
    end,
    loop(LoggableLevels).

%% Registers the process under a convenience name, gets the configured
%% log level, computes all loggable log levels, and starts the service
%% loop.
start(_Options) ->
    erlang:register(?MODULE, erlang:self()),
    LogLevel = hurricane_config_server:get_config(log_level),
    LoggableLevels = get_loggable_levels(LogLevel),
    loop(LoggableLevels).
