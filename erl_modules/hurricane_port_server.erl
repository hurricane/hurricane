-module(hurricane_port_server).

-export([start/1]).

open_port(Cmd) ->
    erlang:open_port({spawn, Cmd}, [{packet, 4}, exit_status, binary]).

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

recv_from_port(Port, TagStack) ->
    receive
        {Port, {exit_status, _Code}} ->
            io:format("~p :: Port ~p died, dying with it...~n", [erlang:self(), Port]),
            NewTagStack = TagStack,
            erlang:exit(kill);
        {Port, {data, Data}} ->
            NewTagStack = handle_port_data(Data, TagStack)
    end,
    {NewTagStack, true}.

recv_next_req(Port) ->
    receive
        {Port, {exit_status, _Code}} ->
            io:format("~p :: Port ~p died, dying with it...~n", [erlang:self(), Port]),
            NewTagStack = [],
            erlang:exit(kill);
        {terminate, _From} ->
            io:format("~p hurricane_port_server terminating...~n", [erlang:self()]),
            NewTagStack = [],
            erlang:exit(normal);
        {request, From, MessageTag, Message} ->
            io:format("~p -> ~p<~p> -> ~p ~p~n", [From, request, MessageTag, erlang:self(), Message]),
            erlang:port_command(
                Port,
                erlang:term_to_binary({request, From, MessageTag, Message})
            ),
            NewTagStack = [];
        {Port, {data, Data}} ->
            NewTagStack = handle_port_data(Data, [])
    end,
    {NewTagStack, false}.

recv_next_step(Port, TagStack) ->
    ExpectedMessageTag = erlang:hd(TagStack),
    receive
        {Port, {exit_status, _Code}} ->
            io:format("~p :: Port ~p died, dying with it...~n", [erlang:self(), Port]),
            NewTagStack = TagStack,
            erlang:exit(kill);
        {response, From, ExpectedMessageTag, Message} ->
            io:format("~p -> ~p<~p> -> ~p ~p~n", [From, response, ExpectedMessageTag, erlang:self(), Message]),
            erlang:port_command(
                Port,
                erlang:term_to_binary({response, From, ExpectedMessageTag, Message})
            ),
            NewTagStack = erlang:tl(TagStack);
        {Port, {data, Data}} ->
            NewTagStack = handle_port_data(Data, TagStack)
    end,
    {NewTagStack, false}.

loop(Port, TagStack, PortReady) ->
    io:format("~p tag stack: ~p~n", [erlang:self(), TagStack]),
    case erlang:length(TagStack) of
        0 ->
            case PortReady of
                false ->
                    {NewTagStack, NewPortReady} = recv_from_port(Port, TagStack);
                true  ->
                    {NewTagStack, NewPortReady} = recv_next_req(Port)
            end;
        _ ->
            {NewTagStack, NewPortReady} = recv_next_step(Port, TagStack)
    end,
    loop(Port, NewTagStack, NewPortReady).

start(Cmd) ->
    Port = open_port(Cmd),
    loop(Port, [], true).
