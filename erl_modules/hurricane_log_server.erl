-module(hurricane_log_server).

-export([start/1, log/2, log/3]).

-define(
    LOG_LEVELS,
    [emergency, alert, critical, error, warning, notice, info, debug]
).

format_local_time() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    io_lib:format(
        "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
        [Year, Month, Day, Hour, Minute, Second]
    ).

log(Level, Message) ->
    ?MODULE ! {log, format_local_time(), Level, Message}.

log(Level, FormatStr, FormatArgs) ->
    Message = io_lib:format(FormatStr, FormatArgs),
    ?MODULE ! {log, format_local_time(), Level, Message}.

get_loggable_levels(ConfigLevel) ->
    [ConfigLevel | lists:takewhile(
        fun(Level) ->
            Level /= ConfigLevel
        end,
        ?LOG_LEVELS
    )].

is_loggable(LoggableLevels, MessageLevel) ->
    RemainingLevels = lists:filter(
        fun(Level) ->
            Level == MessageLevel
        end,
        LoggableLevels
    ),
    erlang:length(RemainingLevels) > 0.

loop(LoggableLevels) ->
    receive
        {log, Timestamp, Level, Message} ->
            case is_loggable(LoggableLevels, Level) of
                true ->
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

start(_Options) ->
    erlang:register(?MODULE, erlang:self()),
    LogLevel = hurricane_config_server:get_config(log_level),
    LoggableLevels = get_loggable_levels(LogLevel),
    loop(LoggableLevels).
