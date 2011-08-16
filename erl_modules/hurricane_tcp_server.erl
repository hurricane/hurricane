-module(hurricane_tcp_server).

-export([start/1, socket_loop/3]).

start(_Args) ->
    ListenPort = hurricane_config_server:get_config(
        hurricane_tcp_server_port
    ),
    {ok, ListenSocket} = gen_tcp:listen(
        ListenPort, [binary, {packet, 4}, {active, false}]
    ),
    acceptor_loop(ListenSocket).

acceptor_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    inet:setopts(Socket, [{active, true}]),
    Pid = erlang:spawn(?MODULE, socket_loop, [Socket, [], true]),
    ok = gen_tcp:controlling_process(Socket, Pid),
    acceptor_loop(ListenSocket).

%% Non-Listener socket functions below

register_with_group(GroupName) ->
    hurricane_log_server:log(
        info,
        "~p registering with group: ~p",
        [erlang:self(), GroupName]
    ),
    pg2:create(GroupName),
    pg2:join(GroupName, erlang:self()).

finish_message_handling(TagStack, MessageType, MessageTag, Term) ->
    case MessageType of
        request -> NewTagStack = [MessageTag | TagStack];
        _       -> NewTagStack = TagStack
    end,
    erlang:spawn(hurricane_message_delegate, send, [erlang:self(), Term]),
    NewTagStack.

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

recv_next_req(Socket) ->
    receive
        {tcp_closed, Port} ->
            gen_tcp:close(Socket),
            hurricane_log_server:log(
                info,
                "~p (~p), closed, dying...",
                [Port, erlang:self()]
            ),
            NewTagStack = [],
            erlang:exit(kill);
        {terminate, _From} ->
            hurricane_log_server:log(
                info,
                "~p tcp server terminating...",
                [erlang:self()]
            ),
            NewTagStack = [],
            erlang:exit(normal);
        {request, From, MessageTag, Message} ->
            hurricane_log_server:log(
                debug,
                "~p -> ~p<~p> -> ~p ~p",
                [From, request, MessageTag, erlang:self(), Message]
            ),
            gen_tcp:send(
                Socket,
                erlang:term_to_binary({request, From, MessageTag, Message})
            ),
            NewTagStack = [];
        {tcp, _Port, Data} ->
            NewTagStack = handle_socket_data(Data, [])
    end,
    {NewTagStack, false}.

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
