%%% Starts and registers the config server. The config server can be
%%% used by any process that wants to access the config (convenience
%%% functions are provided for this).

-module(hurricane_config_server).

-behaviour(gen_server).

-export([get_config/1, get_config/2, start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Called to initialize the process.
init(Args) ->
    {ok, Args}.

%% Used to requests for configuration.
handle_call({get_config, Key}, _From, State) ->
    Config = proplists:get_value(config, State, []),
    Value = proplists:get_value(Key, Config, undefined),
    {reply, Value, State};

%% Handle the reload config message.
handle_call({reload_config}, _From, State) ->
    ConfigPath = proplists:get_value(config_path, State),
    LoadConfigFun = proplists:get_value(load_config_fun, State),
    StateNoConfig = lists:keydelete(config, 1, State),
    Config = erlang:apply(LoadConfigFun, [ConfigPath]),
    NewState = [{config, Config} | StateNoConfig],
    {reply, ok, NewState};

%% Used to handle direct, synchronous requests.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

%% Provides a convenient way for other processes to get the config from
%% the config server (encapsulates the messaging that has to happen and
%% how to receive a reply).
get_config(Key) ->
    gen_server:call(?MODULE, {get_config, Key}).

%% Provides a convenient way for other processes to get the config from
%% the config server (encapsulates the messaging that has to happen and
%% how to receive a reply). Allows a default value to be supplied.
get_config(Key, Default) ->
    case get_config(Key) of
        undefined -> Default;
        Value     -> Value
    end.

%% Registers the process under a convenience name, send a message to
%% load the config, and enters the serve loop.
start(State) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []),
    gen_server:call(?MODULE, {reload_config}).
