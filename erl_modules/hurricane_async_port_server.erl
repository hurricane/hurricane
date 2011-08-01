-module(hurricane_async_port_server).

-export([start/1]).

loop(Port) ->
    receive
        {Port, {data, Data}} ->
            {RequestOrResponse, Destination, MessageType, Message} = 
                erlang:binary_to_term(Data),
            case erlang:is_pid(Destination) of
                true -> SendTo = Destination;
                _    -> SendTo = hurricane_utils:get_best_pid(Destination)
            end,
            io:format("~p <- ~p ~p<~p> ~p~n", [SendTo, erlang:self(), RequestOrResponse, MessageType, Message]),
            SendTo ! {RequestOrResponse, erlang:self(), MessageType, Message};
        {RequestOrResponse, From, MessageType, Message} ->
            io:format("~p -> ~p ~p<~p> ~p~n", [From, erlang:self(), RequestOrResponse, MessageType, Message]),
            erlang:port_command(
                Port,
                erlang:term_to_binary(
                    {RequestOrResponse, From, MessageType, Message}
                )
            );
        Other ->
            io:format("??? -> ~p ~p~n", [erlang:self(), Other])
    end,
    loop(Port).

start(Cmd) ->
    Port = erlang:open_port({spawn, Cmd}, [{packet, 4}, exit_status, binary]),
    loop(Port).
