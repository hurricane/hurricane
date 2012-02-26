%%% Handles distribution, node naming, and connectivity logic between
%%% nodes.

-module(hurricane_distribution_manager).

-behaviour(gen_server).

-export([start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Names the node, sets the magic cookie, and connects to all.
%% configured nodes.
init(Args) ->
    NodeName = proplists:get_value(node_name, Args),
    net_kernel:start([NodeName, longnames]),

    hurricane_log_server:log(
        info,
        "The name of this Hurricane node is: ~p",
        [erlang:node()]
    ),

    MagicCookie = proplists:get_value(magic_cookie, Args),
    try erlang:set_cookie(erlang:node(), MagicCookie) of
         _ -> ok
    catch ErrorType:Error ->
        hurricane_log_server:log(
            error,
            "Failed to set magic cookie. "
            "This means that distribution support "
            "probably failed to start. Check that epmd is running "
            "and restart Hurricane: "
            "Error: ~p, ErrorType: ~p",
            [Error, ErrorType]
        ),
        io:format("Erorr")
    end,

    lists:map(
        fun(Node) ->
            ConnectResult = net_kernel:connect_node(Node),
            handle_connect_result(Node, ConnectResult)
        end,
        proplists:get_value(connect_nodes, Args, [])
    ),
    {ok, []}.

%% Used to handle direct, synchronous requests.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% Handle all other asynchronous messages.
handle_cast(_Request, State) ->
    {noreply, State}.

%% Used to handle direct process messages.
handle_info(_Info, State) ->
    {noreply, State}.

%% Called before exiting--clean up happens here.
terminate(_Reason, _State) ->
    ok.

%% Called to upgrade the currently running code.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Handles alerting the administrator when a connection to another
%% node could not be made.
handle_connect_result(NodeName, ConnectResult) ->
    case ConnectResult of
        true ->
            ok;
        _ ->
            hurricane_log_server:log(
                error,
                "~p failed to connect to node: ~p (~p)",
                [?MODULE, NodeName, ConnectResult]
            )
    end.

%% Starts the distribution process gen server.
start(Args) ->
    gen_server:start_link(?MODULE, Args, []).
