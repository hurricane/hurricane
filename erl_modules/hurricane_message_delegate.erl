%%% Provides multiplexing with multiple messages. The delegate applies
%%% common logic to all messaging and provides a way for each message
%%% to perform blocking operations in its own process. The most obvious
%%% need is the ability to wait for messages while applying timeout
%%% logic.

-module(hurricane_message_delegate).

-export([send/2]).

%% Takes a message, optionally applies a default timeout, and passes it
%% on to the main send logic.
send(Source, {MessageType, Destination, MessageTag, Message}) ->
    Timeout = 10000,
    send(Source, MessageType, Destination, MessageTag, Message, Timeout);
send(Source, {MessageType, Destination, MessageTag, Message, Timeout}) ->
    send(Source, MessageType, Destination, MessageTag, Message, Timeout).

%% Takes all of the information having to do with a message, chooses a
%% Pid (if needed), send the message, and then receives the response.
send(Source, MessageType, Destination, MessageTag, Message, Timeout) ->
    case erlang:is_pid(Destination) of
        true -> SendTo = Destination;
        _    -> SendTo = hurricane_utils:get_best_pid(Destination)
    end,
    hurricane_log_server:log(
        debug,
        "~p <- ~p<~p> <- ~p ~p",
        [SendTo, MessageType, MessageTag, Source, Message]
    ),
    case SendTo of
        {error, Reason} ->
            hurricane_log_server:log(
                error,
                "Cannot send message to requested destination (~p): ~p",
                [Destination, Reason]
            ),
            Source ! {response, erlang:self(), MessageTag, error};
        _ ->
            SendTo ! {MessageType, erlang:self(), MessageTag, Message},
            recv(Source, MessageType, MessageTag, Timeout)
    end.

%% If the message is a request, then the delegate will wait for a
%% response up until Timeout. Once that time is reached, a timeout
%% message is sent back to the message originator.
recv(Source, request, MessageTag, Timeout) ->
    receive
        {response, From, MessageTag, Message} ->
            Source ! {response, From, MessageTag, Message}
    after Timeout ->
        Source ! {response, erlang:self(), MessageTag, timeout}
    end;
recv(_Source, _MessageType, _MessageTag, _Timeout) ->
    ok.
