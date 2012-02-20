%%% This is the module that exposes an HTTP interface to Hurricane.
%%% Every message that is received will be sent to the http_handler
%%% group of processes to handle. As such, Hurricane is quite
%%% suitable for use as a generic web application server.

-module(hurricane_http_server).

-include_lib("kernel/include/file.hrl").

-export([start/1]).

%% Sets up an infinite loop for the managing process so that it never
%% has to exit (and thus be restarted by the supervisor).
loop() ->
    receive
        _ -> ok
    end,
    loop().

%% Starts Mochiweb listening on the port specified in the config (80 is
%% the default). Defines what function Mochiweb should call back to.
%% Enters the service loop.
start(Options) ->
    application:start(mochiweb),
    ListenPort = proplists:get_value(listen_port, Options, 80),
    ServerName = proplists:get_value(server_name, Options, "localhost"),
    HandlerGroup = proplists:get_value(handler_group, Options, http_handler),
    ResponseTimeout = proplists:get_value(response_timeout, Options, 10000),
    StaticDir = proplists:get_value(static_dir, Options, undefined),

    HttpHandler = fun(Request) ->
        case StaticDir of
            undefined -> FileServed = false;
            _ ->
                FullPath = [StaticDir, Request:get(path)],
                FileServed = try_serve_file(Request, FullPath, [])
        end,
        case FileServed of
            false ->
                HandlerPid = hurricane_utils:get_best_pid(HandlerGroup),
                SendData = [
                    {listen_port, ListenPort},
                    {server_name, ServerName},
                    {scheme, Request:get(scheme)},
                    {version, Request:get(version)},
                    {method, Request:get(method)},
                    {headers, mochiweb_headers:to_list(Request:get(headers))},
                    {path, Request:get(raw_path)},
                    {body, Request:recv_body(Request:get(body_length))}
                ],
                HandlerPid ! {request, erlang:self(), http_request, SendData},
                receive
                    {response, _FromPid, http_request, Data} -> Request:respond(Data)
                after ResponseTimeout ->
                    Request:respond({504, [], "Internal Timeout."})
                end;
            _ -> ok
        end
    end,

    mochiweb_http:start(
        [
            {name, proplists:get_value(name, Options, default)},
            {loop, HttpHandler},
            {port, ListenPort}
        ]
    ),
    loop().

%% Serve the specified file with the given extra headers. Return true on
%% success, false on failure. Guesses the mime type of the file, falling
%% back to text/plain. Handle IMS headers to avoid re-serving cached
%% files.
try_serve_file(Request, File, ExtraHeaders) ->
    case file:read_file_info(File) of
        {ok, FileInfo} ->
            LastModified = httpd_util:rfc1123_date(FileInfo#file_info.mtime),
            case Request:get_header_value("if-modified-since") of
                LastModified ->
                    Request:respond({304, ExtraHeaders, ""});
                _ ->
                    case file:open(File, [read, raw, binary, read_ahead]) of
                        {ok, IoDevice} ->
                            ContentType = mochiweb_util:guess_mime(File),
                            _Res = Request:ok({
                                ContentType,
                                [{"last-modified", LastModified} | ExtraHeaders],
                                {file, IoDevice}}
                            ),
                            ok = file:close(IoDevice),
                            true;
                        _ ->
                            false
                    end
            end;
        {error, _Why} ->
            false
    end.
