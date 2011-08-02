-module(hurricane_async_tcp_server).

-export([start/0, socket_loop/1]).

start() ->
    ListenPort = hurricane_config_server:get_config(
        hurricane_async_tcp_server_port
    ),
    {ok, ListenSocket} = gen_tcp:listen(
        ListenPort, [binary, {packet, 4}, {active, false}]
    ),
    acceptor_loop(ListenSocket).

acceptor_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    inet:setopts(Socket, [{active, true}]),
    Pid = erlang:spawn(hurricane_async_tcp_server, socket_loop, [Socket]),
    ok = gen_tcp:controlling_process(Socket, Pid),
    acceptor_loop(ListenSocket).

handle_tcp_data(Data) ->
    case erlang:binary_to_term(Data) of
        {RequestOrResponse, Destination, MessageTag, Message} ->
            case erlang:is_pid(Destination) of
                true -> SendTo = Destination;
                _    -> SendTo = hurricane_utils:get_best_pid(Destination)
            end,
            io:format("~p <- ~p ~p<~p> ~p~n", [SendTo, erlang:self(), RequestOrResponse, MessageTag, Message]),
            SendTo ! {RequestOrResponse, erlang:self(), MessageTag, Message};
        {register_with_group, GroupName} ->
            io:format("~p registering with group: ~p~n", [erlang:self(), GroupName]),
            pg2:create(GroupName),
            pg2:join(GroupName, erlang:self())
    end.


socket_loop(Socket) ->
    receive
        {tcp, _Port, Data} ->
            handle_tcp_data(Data);
        {RequestOrResponse, From, MessageTag, Message} ->
            io:format("~p -> ~p ~p<~p> ~p~n", [From, erlang:self(), RequestOrResponse, MessageTag, Message]),
            gen_tcp:send(
                Socket,
                erlang:term_to_binary({RequestOrResponse, From, MessageTag, Message})
            );
        {tcp_closed, Port} ->
            gen_tcp:close(Socket),
            io:format("~p (~p), closed, dying...~n", [Port, erlang:self()]),
            erlang:exit(kill);
        Other ->
            io:format("~p~n", [Other])
    end,
    socket_loop(Socket).
