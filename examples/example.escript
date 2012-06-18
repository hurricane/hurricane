#!/usr/bin/env escript
%%! -smp enable

%% Example program that uses Hurricane from an Erlang client.
main(_Args) ->
    Host = "localhost",
    Port = 3000,
    {ok, Socket} = gen_tcp:connect(
        Host, Port, [binary, {packet, 4}, {active, false}]
    ),
    Request = {request, time_server, xxx_some_tag_xxx, xxx_some_value_xxx},
    io:format("request:  ~p~n", [Request]),

    BinaryRequest = erlang:term_to_binary(Request),
    ok = gen_tcp:send(Socket, BinaryRequest),
    {ok, BinaryResponse} = gen_tcp:recv(Socket, 0),

    Response = erlang:binary_to_term(BinaryResponse),
    io:format("response: ~p~n", [Response]).
