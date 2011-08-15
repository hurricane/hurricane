-module(hurricane_message_delegate).

-export([send/2]).

send(Source, {MessageType, Destination, MessageTag, Message}) ->
    Timeout = 10000,
    send(Source, MessageType, Destination, MessageTag, Message, Timeout);
send(Source, {MessageType, Destination, MessageTag, Message, Timeout}) ->
    send(Source, MessageType, Destination, MessageTag, Message, Timeout).

send(Source, MessageType, Destination, MessageTag, Message, Timeout) ->
    case erlang:is_pid(Destination) of
        true -> SendTo = Destination;
        _    -> SendTo = hurricane_utils:get_best_pid(Destination)
    end,
    io:format("~p <- ~p<~p> <- ~p ~p~n", [SendTo, MessageType, MessageTag, Source, Message]),
    SendTo ! {MessageType, erlang:self(), MessageTag, Message},
    recv(Source, MessageType, MessageTag, Timeout).

recv(Source, request, MessageTag, Timeout) ->
    receive
        {response, From, MessageTag, Message} ->
            Source ! {response, From, MessageTag, Message}
    after Timeout ->
        Source ! {response, erlang:self(), MessageTag, timeout}
    end;
recv(_Source, _MessageType, _MessageTag, _Timeout) ->
    ok.
