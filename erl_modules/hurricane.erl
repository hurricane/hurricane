-module(hurricane).

-export([start/1]).

start(Options) ->
    application:start(sasl),
    hurricane_config_server:start(Options),
    erlang:spawn_link(hurricane_async_tcp_server, start, []),
    application:start(mochiweb),
    mochiweb_http:start([{name, ?MODULE}, {loop, fun http_handler/1}, {port, 8000}]).

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
    after 5000 ->
        Request:respond({500, [], "Internal Server Error -- Timeout."})
    end.
