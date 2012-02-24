%%% Handles logging of all events in the system. Exposes a service and
%%% convenience functions.

-module(hurricane_log_server).

-behaviour(gen_server).

-export([start/1, log/2, log/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(
    LOG_LEVELS,
    [emergency, alert, critical, error, warning, notice, info, debug]
).

%% Called to initialize the process.
init(_Args) ->
    LogLevel = hurricane_config_server:get_config(log_level),
    LoggableLevels = get_loggable_levels(LogLevel),
    {ok, LoggableLevels}.

%% Used to handle direct, synchronous requests.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% Log asynchronous log messages that need to be formatted.
handle_cast({log, Timestamp, Level, FormatStr, FormatArgs}, LoggableLevels) ->
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
    end,
    {noreply, LoggableLevels};

%% Log asynchronous log strings.
handle_cast({log, Timestamp, Level, Message}, LoggableLevels) ->
    case is_loggable(LoggableLevels, Level) of
        true ->
            io:format(
                standard_error,
                "[~s] [~s] ~s~n",
                [Timestamp, Level, Message]
            );
        _ -> ok
    end,
    {noreply, LoggableLevels};

%% Handle all other asynchronous messages.
handle_cast(_Request, State) ->
    {noreply, State}.

%% Used to handle direct process messages.
handle_info(_Info, State) ->
    {noreply, State}.

%% Called before exiting--clean up happens here.
terminate(_Reason, _State) ->
    ok.

%% Called to upgrade the currently running code.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Formats the current time in the format YYYY:mm:dd HH:MM:SS.
format_local_time() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    io_lib:format(
        "~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
        [Year, Month, Day, Hour, Minute, Second]
    ).

%% Convenience function to log an already-formatted message.
log(Level, Message) ->
    gen_server:cast(
        ?MODULE,
        {log, format_local_time(), Level, Message}
    ).

%% Convenience function to log a message that needs to be formatted
%% (formatting is handled by io_lib).
log(Level, FormatStr, FormatArgs) ->
    gen_server:cast(
        ?MODULE,
        {log, format_local_time(), Level, FormatStr, FormatArgs}
    ).

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

%% Register the log process and run it.
start(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).
