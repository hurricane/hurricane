%%% Provides the ability to message with external processes over
%%% TCP. Does not manage processes (if they are connecting over a
%%% network, they are expected to manage themselves). Supports the
%%% scatter-gather pattern of messaging transparently.

-module(hurricane_tcp_server).

-export([start/1, socket_loop/3]).

%% Starts the listener socket on the configured port (default 81).
%% Enters the acceptor loop.
start(Options) ->
    ListenPort = proplists:get_value(listen_port, Options, 81),
    {ok, ListenSocket} = gen_tcp:listen(
        ListenPort, [binary, {packet, 4}, {active, false}]
    ),
    acceptor_loop(ListenSocket).

%% Accepts new connections and spawns new processes to deal with them.
%% This pattern makes it so that errors on a socket do not crash the
%% entire server and are isolated, as well as all running in parallel.
acceptor_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    inet:setopts(Socket, [{active, true}]),
    Pid = erlang:spawn(?MODULE, socket_loop, [Socket, [], true]),
    ok = gen_tcp:controlling_process(Socket, Pid),
    acceptor_loop(ListenSocket).

%% Non-Listener socket functions below

%% Allows a process to register with a messaging group over TCP. If the
%% process dies, it is automatically removed from the group.
register_with_group(GroupName) ->
    hurricane_log_server:log(
        info,
        "~p registering with group: ~p",
        [erlang:self(), GroupName]
    ),
    pg2:create(GroupName),
    pg2:join(GroupName, erlang:self()).

%% Common logic that is to be performed whenever messaging data from
%% the socket is being handled.
finish_message_handling(TagStack, MessageType, MessageTag, Term) ->
    case MessageType of
        request -> NewTagStack = [MessageTag | TagStack];
        _       -> NewTagStack = TagStack
    end,
    erlang:spawn(hurricane_message_delegate, send, [erlang:self(), Term]),
    NewTagStack.

%% Handles data that comes in from the socket. Messages can be sent,
%% groups can be registered with, etc.
handle_socket_data(Data, TagStack) ->
    Term = erlang:binary_to_term(Data),
    case Term of
        {register_with_group, GroupName} ->
            register_with_group(GroupName),
            NewTagStack = TagStack;
        {MessageType, _Destination, MessageTag, _Message} ->
            NewTagStack = finish_message_handling(TagStack, MessageType, MessageTag, Term);
        {MessageType, _Destination, MessageTag, _Message, _Timeout} ->
            NewTagStack = finish_message_handling(TagStack, MessageType, MessageTag, Term)
    end,
    NewTagStack.

%% Receives data from the socket only. Uses handle_socket_data/2 to
%% actually act on the data received.
recv_from_socket(Socket, TagStack) ->
    receive
        {tcp_closed, Port} ->
            gen_tcp:close(Socket),
            hurricane_log_server:log(
                info,
                "~p (~p), closed, dying...",
                [Port, erlang:self()]
            ),
            NewTagStack = TagStack,
            erlang:exit(kill);
        {tcp, _Port, Data} ->
            NewTagStack = handle_socket_data(Data, TagStack)
    end,
    {NewTagStack, true}.

%% Receives the next request from the rest of the Hurricane system to
%% this process. Handles any errors that may have come up in the mean
%% time by crashing fast.
recv_next_req(Socket) ->
    receive
        {tcp_closed, Port} ->
            gen_tcp:close(Socket),
            hurricane_log_server:log(
                info,
                "~p (~p), closed, dying...",
                [Port, erlang:self()]
            ),
            NewSocketReady = false,
            NewTagStack = [],
            erlang:exit(kill);
        {terminate, _From} ->
            hurricane_log_server:log(
                info,
                "~p tcp server terminating...",
                [erlang:self()]
            ),
            NewSocketReady = false,
            NewTagStack = [],
            erlang:exit(normal);
        {MessageType, From, MessageTag, Message} ->
            hurricane_log_server:log(
                debug,
                "~p -> ~p<~p> -> ~p ~p",
                [From, MessageType, MessageTag, erlang:self(), Message]
            ),
            gen_tcp:send(
                Socket,
                erlang:term_to_binary({MessageType, From, MessageTag, Message})
            ),
            case MessageType of
                request -> NewSocketReady = false;
                _       -> NewSocketReady = true
            end,
            NewTagStack = [];
        {tcp, _Port, Data} ->
            NewSocketReady = false,
            NewTagStack = handle_socket_data(Data, [])
    end,
    {NewTagStack, NewSocketReady}.

%% Receives both responses to earlier requests from the external
%% process and requests from the external process. Manages the tag
%% stack accordingly. This is important as servicing a single request
%% may require the external process to ask for data in complicated
%% ways, and ordering should not break.
recv_next_step(Socket, TagStack) ->
    ExpectedMessageTag = erlang:hd(TagStack),
    receive
        {tcp_closed, Port} ->
            gen_tcp:close(Socket),
            hurricane_log_server:log(
                info,
                "~p (~p), closed, dying...",
                [Port, erlang:self()]
            ),
            NewTagStack = TagStack,
            erlang:exit(kill);
        {response, From, ExpectedMessageTag, Message} ->
            hurricane_log_server:log(
                debug,
                "~p -> ~p<~p> -> ~p ~p",
                [From, response, ExpectedMessageTag, erlang:self(), Message]
            ),
            gen_tcp:send(
                Socket,
                erlang:term_to_binary({response, From, ExpectedMessageTag, Message})
            ),
            NewTagStack = erlang:tl(TagStack);
        {tcp, _Port, Data} ->
            NewTagStack = handle_socket_data(Data, TagStack)
    end,
    {NewTagStack, false}.

%% The service loop enforces semantics of logic ordering. New, external
%% requests can only be processed once the tag stack is empty and the
%% external process has replied to the original request. Other such
%% details are handled in the logic branching in this loop.
socket_loop(Socket, TagStack, SocketReady) ->
    hurricane_log_server:log(
        debug,
        "~p tag stack: ~p",
        [erlang:self(), TagStack]
    ),
    case erlang:length(TagStack) of
        0 ->
            case SocketReady of
                false ->
                    {NewTagStack, NewSocketReady} = recv_from_socket(Socket, TagStack);
                true  ->
                    {NewTagStack, NewSocketReady} = recv_next_req(Socket)
            end;
        _ ->
            {NewTagStack, NewSocketReady} = recv_next_step(Socket, TagStack)
    end,
    socket_loop(Socket, NewTagStack, NewSocketReady).
