-module(findNodes).

%-define(KeySize,4).
-define(KeySize,8).
-export([find_node/3, find_data/3]).

printList([]) -> io:format("~n");
printList( [L|List]) ->
    io:format("~p ", [L]),
    printList(List).

removeDuplicates(List) ->
    Set = sets:from_list(List),
    sets:to_list(Set).

recieve_responses(Returned, Time, ToFind) ->
    receive
        {notfound, unavailable} ->
            unavailable,
            recieve_responses(Returned, Time, ToFind);
        {notfound, KNodes} ->
            recieve_responses(KNodes ++ Returned, Time, ToFind);
        {node_found, InfoTup} ->
            {node_found, InfoTup};
        {data_found, InfoTup} ->
            {data_found, InfoTup}
        %This might be VERY DUMB
        %Any ->
         %   self() ! Any
    after
        Time ->
            Closest = lists:sort( fun({NodeId1, _, _}, {NodeId2,_,_}) -> (NodeId1 bxor ToFind) > (NodeId2 bxor ToFind) end, removeDuplicates(Returned) ),

            KClosest = lists:sublist(Closest, ?KeySize),

            {notfound, KClosest}
    end.


send_rpc(Node_Info, _, ToFind, []) ->
    {MyId, _, _} = Node_Info,
    %Might have to flush.
    recieve_responses([], 2000, ToFind);

%% Node_Info
send_rpc(Node_Info, Rpc, ToFind, [N | Nodes]) ->
    {_, Ip, _} = N,
    %io:display(Ip)
    Ip ! {Rpc, Node_Info, ToFind},
    %N ! {Rpc, Node_Info, ToFind},
    send_rpc(Node_Info, Rpc, ToFind, Nodes).

find_node(Node_Info, ToFind, KNodes) ->
    Closest = send_rpc(Node_Info, find_node, ToFind, KNodes),
    case Closest of
        unavailable ->
            unavailable;
        {notfound, []} ->
            io:format("Damn. Empty List~n"),
            [];
        {notfound, RetNodes} ->
            find_node(Node_Info, ToFind, RetNodes);
        {node_found, InfoTup} ->
            io:format("~p Node Found~n", [InfoTup]),
            InfoTup
    end.

find_data(Node_Info, DataKey, KNodes) ->
    Closest = send_rpc(Node_Info, find_data, DataKey, KNodes),
    case Closest of
        unavailable ->
            unavailable;
        {notfound, []} ->
            io:format("Damn. Empty List"),
            [];
        {notfound, RetNodes} ->
            find_data(Node_Info, DataKey, RetNodes);
        {data_found, InfoTup} ->
            io:format("~p Data Found~n", [InfoTup]),
            InfoTup
    end.
