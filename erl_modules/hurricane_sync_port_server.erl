-module(hurricane_sync_port_server).

-export([start/1]).

handle_port_data(Data, TagStack) ->
    {Type, Destination, MessageTag, Message} = erlang:binary_to_term(Data),
    case erlang:is_pid(Destination) of
        true -> SendTo = Destination;
        _    -> SendTo = hurricane_utils:get_best_pid(Destination)
    end,
    case Type of
        request -> NewTagStack = [MessageTag | TagStack];
        _       -> NewTagStack = TagStack
    end,
    io:format("~p <- ~p<~p> <- ~p ~p~n", [SendTo, Type, MessageTag, erlang:self(), Message]),
    SendTo ! {Type, erlang:self(), MessageTag, Message},
    NewTagStack.

recv_from_port(Port, TagStack) ->
    receive
        {Port, {data, Data}} ->
            NewTagStack = handle_port_data(Data, TagStack)
    end,
    {NewTagStack, true}.

recv_next_req(Port) ->
    receive
        {terminate, _From} ->
            io:format("~p hurricane_sync_port_server terminating...~n", [erlang:self()]),
            erlang:exit(normal);
        {request, From, MessageTag, Message} ->
            io:format("~p -> ~p<~p> -> ~p ~p~n", [From, request, MessageTag, erlang:self(), Message]),
            erlang:port_command(
                Port,
                erlang:term_to_binary({request, From, MessageTag, Message})
            )
    end,
    {[], false}.

recv_next_step(Port, TagStack) ->
    ExpectedMessageTag = erlang:hd(TagStack),
    receive
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
    Port = erlang:open_port({spawn, Cmd}, [{packet, 4}, exit_status, binary]),
    loop(Port, [], true).
