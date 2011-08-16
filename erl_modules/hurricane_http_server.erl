-module(hurricane_http_server).

-export([start/1]).

http_handler(Request) ->
    HandlerPid = hurricane_utils:get_best_pid(http_handler),
    SendData = [
        {scheme, Request:get(scheme)},
        {version, Request:get(version)},
        {method, Request:get(method)},
        {headers, mochiweb_headers:to_list(Request:get(headers))},
        {raw_path, Request:get(raw_path)},
        {path, Request:get(path)},
        {params, Request:parse_qs()},
        {body, Request:recv_body(Request:get(body_length))}
    ],
    HandlerPid ! {request, erlang:self(), http_request, SendData},
    receive
        {response, _FromPid, http_request, Data} -> Request:respond(Data)
    after 10000 ->
        Request:respond({500, [], "Internal Server Error -- Timeout."})
    end.

loop() ->
    receive
        _ -> ok
    end,
    loop().

start(Options) ->
    application:start(mochiweb),
    ListenPort = proplists:get_value(listen_port, Options, 80),
    mochiweb_http:start(
        [{name, ?MODULE}, {loop, fun http_handler/1}, {port, ListenPort}]
    ),
    loop().
