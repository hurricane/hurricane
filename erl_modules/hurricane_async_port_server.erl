-module(hurricane_async_port_server).

-export([start/1]).

loop(Port, WaitingOnNum) ->
    receive
        {terminate, _From} when WaitingOnNum < 1 ->
            io:format("~p hurricane_async_port_server terminating...~n", [erlang:self()]),
            NewWaitingOnNum = 0,
            erlang:exit(normal);
        {Port, {data, Data}} ->
            {RequestOrResponse, Destination, MessageType, Message} = 
                erlang:binary_to_term(Data),
            case erlang:is_pid(Destination) of
                true -> SendTo = Destination;
                _    -> SendTo = hurricane_utils:get_best_pid(Destination)
            end,
            io:format("~p <- ~p ~p<~p> ~p~n", [SendTo, erlang:self(), RequestOrResponse, MessageType, Message]),
            SendTo ! {RequestOrResponse, erlang:self(), MessageType, Message},
            NewWaitingOnNum = WaitingOnNum - 1;
        {RequestOrResponse, From, MessageType, Message} ->
            io:format("~p -> ~p ~p<~p> ~p~n", [From, erlang:self(), RequestOrResponse, MessageType, Message]),
            erlang:port_command(
                Port,
                erlang:term_to_binary(
                    {RequestOrResponse, From, MessageType, Message}
                )
            ),
            NewWaitingOnNum = WaitingOnNum + 1;
        Other ->
            io:format("??? -> ~p ~p~n", [erlang:self(), Other]),
            NewWaitingOnNum = WaitingOnNum
    end,
    loop(Port, NewWaitingOnNum).

start(Cmd) ->
    Port = erlang:open_port({spawn, Cmd}, [{packet, 4}, exit_status, binary]),
    loop(Port, 0).
