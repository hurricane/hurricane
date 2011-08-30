%%% Handles distribution, node naming, and connectivity logic between
%%% nodes.

-module(hurricane_distribution_manager).

-export([start/1]).

%% Service loop, which does nothing.
loop() ->
    receive
        Other -> 
            hurricane_log_server:log(
                warning,
                "~p (~p) received unexpected message: ~p",
                [?MODULE, erlang:self(), Other]
            )
    end,
    loop().

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

%% Registers the process under a convenience name, names the node,
%% sets the magic cookie, connects to all configured nodes, and enters
%% a service loop.
start(Options) ->
    erlang:register(?MODULE, erlang:self()),
    pg2:start_link(),

    NodeName = proplists:get_value(node_name, Options),
    net_kernel:start([NodeName]),

    hurricane_log_server:log(
        info,
        "The name of this Hurricane node is: ~p",
        [erlang:node()]
    ),

    MagicCookie = proplists:get_value(magic_cookie, Options),
    erlang:set_cookie(erlang:node(), MagicCookie),

    lists:map(
        fun(Node) ->
            ConnectResult = net_kernel:connect_node(Node),
            handle_connect_result(Node, ConnectResult)
        end,
        proplists:get_value(connect_nodes, Options, [])
    ),
    loop().
