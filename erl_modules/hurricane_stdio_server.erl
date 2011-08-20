%%% Provides the ability to message with external processes over
%%% Standard I/O. Also manages processes (they are restarted if they do
%%% not start up fast enough or respond fast enough). Supports the
%%% scatter-gather pattern of messaging transparently.

-module(hurricane_stdio_server).

-export([start/1]).

%% Starts a process and hooks up to it over Standard I/O.
open_port(Cmd) ->
    erlang:open_port({spawn, Cmd}, [{packet, 4}, exit_status, binary]).

%% Receives data from the external process and handles it. If the
%% message is a request, it needs to be kept track of on the tag stack.
%% This is done to correctly enforce scatter-gather ordering semantics.
handle_port_data(Data, TagStack) ->
    Term = erlang:binary_to_term(Data),
    case Term of
        {MessageType, _Destination, MessageTag, _Message} -> ok;
        {MessageType, _Destination, MessageTag, _Message, _Timeout} -> ok
    end,
    case MessageType of
        request -> NewTagStack = [MessageTag | TagStack];
        _       -> NewTagStack = TagStack
    end,
    erlang:spawn(hurricane_message_delegate, send, [erlang:self(), Term]),
    NewTagStack.

%% Receives data from the external process only. Handles faults by
%% crashing fast; uses handle_port_data/2 to actually act on the data
%% received.
recv_from_port(Port, TagStack, PortTimeout) ->
    receive
        {Port, {exit_status, _Code}} ->
            hurricane_log_server:log(
                error,
                "~p :: Port ~p died, dying with it...",
                [erlang:self(), Port]
            ),
            NewTagStack = TagStack,
            erlang:exit(kill);
        {Port, {data, Data}} ->
            NewTagStack = handle_port_data(Data, TagStack)
    after PortTimeout ->
        NewTagStack = TagStack,
        erlang:exit(timeout)
    end,
    {NewTagStack, true}.

%% Receives the next request from the rest of the Hurricane system to
%% this process. Handles any errors that may have come up in the mean
%% time by crashing fast.
recv_next_req(Port) ->
    receive
        {Port, {exit_status, _Code}} ->
            hurricane_log_server:log(
                error,
                "~p :: Port ~p died, dying with it...",
                [erlang:self(), Port]
            ),
            NewPortReady = false,
            NewTagStack = [],
            erlang:exit(kill);
        {terminate, _From} ->
            hurricane_log_server:log(
                info,
                "~p stdio server terminating...",
                [erlang:self()]
            ),
            NewPortReady = false,
            NewTagStack = [],
            erlang:exit(normal);
        {MessageType, From, MessageTag, Message} ->
            hurricane_log_server:log(
                debug,
                "~p -> ~p<~p> -> ~p ~p",
                [From, MessageType, MessageTag, erlang:self(), Message]
            ),
            erlang:port_command(
                Port,
                erlang:term_to_binary({MessageType, From, MessageTag, Message})
            ),
            case MessageType of
                request -> NewPortReady = false;
                _       -> NewPortReady = true
            end,
            NewTagStack = [];
        {Port, {data, Data}} ->
            NewPortReady = false,
            NewTagStack = handle_port_data(Data, [])
    end,
    {NewTagStack, NewPortReady}.

%% Receives both responses to earlier requests from the external
%% process and requests from the external process. Manages the tag
%% stack accordingly. This is important as servicing a single request
%% may require the external process to ask for data in complicated
%% ways, and ordering should not break.
recv_next_step(Port, TagStack) ->
    ExpectedMessageTag = erlang:hd(TagStack),
    receive
        {Port, {exit_status, _Code}} ->
            hurricane_log_server:log(
                error,
                "~p :: Port ~p died, dying with it...",
                [erlang:self(), Port]
            ),
            NewTagStack = TagStack,
            erlang:exit(kill);
        {response, From, ExpectedMessageTag, Message} ->
            hurricane_log_server:log(
                debug,
                "~p -> ~p<~p> -> ~p ~p",
                [From, response, ExpectedMessageTag, erlang:self(), Message]
            ),
            erlang:port_command(
                Port,
                erlang:term_to_binary({response, From, ExpectedMessageTag, Message})
            ),
            NewTagStack = erlang:tl(TagStack);
        {Port, {data, Data}} ->
            NewTagStack = handle_port_data(Data, TagStack)
    end,
    {NewTagStack, false}.

%% The service loop enforces semantics of logic ordering. New, external
%% requests can only be processed once the tag stack is empty and the
%% external process has replied to the original request. Other such
%% details are handled in the logic branching in this loop.
loop(Port, TagStack, PortReady, PortTimeout) ->
    hurricane_log_server:log(
        debug,
        "~p tag stack: ~p",
        [erlang:self(), TagStack]
    ),
    case erlang:length(TagStack) of
        0 ->
            case PortReady of
                false ->
                    {NewTagStack, NewPortReady} = recv_from_port(Port, TagStack, PortTimeout);
                true  ->
                    {NewTagStack, NewPortReady} = recv_next_req(Port)
            end;
        _ ->
            {NewTagStack, NewPortReady} = recv_next_step(Port, TagStack)
    end,
    loop(Port, NewTagStack, NewPortReady, PortTimeout).

%% Starts the external process and waits (for the configured amount of
%% time) for the external process to say it is ready. If this times
%% out, the process crashes fast.
init_port(Cmd, PortStartupTimeout) ->
    Port = open_port(Cmd),
    receive
        {Port, {data, Data}} ->
            Term = erlang:binary_to_term(Data),
            case Term of
                {ready} -> ok;
                _ -> erlang:exit(bad_port_start)
            end
    after PortStartupTimeout ->
        erlang:exit(port_start_timeout)
    end,
    Port.

%% Starts this module. Starts the port, enforces its ready timeout.
%% Registers the process with a named group (as configured). Enters the
%% service loop.
start(Options) ->
    Cmd = proplists:get_value(cmd, Options),
    PortStartupTimeout = proplists:get_value(port_startup_timeout, Options, 10000),
    PortTimeout = proplists:get_value(port_timeout, Options, 10000),
    Port = init_port(Cmd, PortStartupTimeout),

    GroupName = proplists:get_value(group_name, Options),
    pg2:create(GroupName),
    pg2:join(GroupName, erlang:self()),

    loop(Port, [], true, PortTimeout).
