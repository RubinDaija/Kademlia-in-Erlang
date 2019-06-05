-module(node).

%-include("kbuckets.erl").
%-include("findNodes.erl").
-export([init/3 ]).

printList([]) -> io:format("~n");
printList( [L|List]) ->
    io:format("~p ", [L]),
    printList(List).

init(Node_Id, Node_Ip, BootstrapServer) ->
    Node_Info = {Node_Id, self(), Node_Ip},
    kbuckets:setUP(),
    requestKBuckets(Node_Id, BootstrapServer),
    receiveKBuckets(Node_Id),
    kbuckets:store(Node_Id*2, Node_Id),
    %io:format("~p~n", [Node_Id]),
    %kbuckets:printAllNodesInTable(),
    loop(Node_Info).

requestKBuckets(Node_Id, BootstrapServer) ->
    BootstrapServer ! {kbucket, self(), Node_Id}.

addElems([], _) -> done;
addElems([B | Buckets], MyId) ->
    {Node_Id, Ip, Port} = B,
    kbuckets:checkInsertNewNode(Node_Id, Ip, Port, MyId),
    addElems(Buckets, MyId).

receiveKBuckets(MyId) ->
    receive
        {kresp, Buckets} ->
            addElems(Buckets, MyId)
    end.

%init(IP, Port) ->
 %   Digest = crypto:hash(md5, IP),
  %  Node_Info = {crypto:bytes_to_integer(Digest), IP, Port},
   % kbuckets:setUP(),
    %loop(Node_Info).

%This function is called by the node that initiates the find request
find(Node_Info, ToFind) -> %Resquester_Info, ToFind) ->
    %{Requester_ID, Requester_IP, Requester_Port} = Resquester_Info,
    {MyId, _, _} = Node_Info,
    %K-Nodes = kbuckets:checkInsertNewNode(Requester_ID, Requester_IP, Requester_Port, MyId),
    [K | Nodes] = kbuckets:find_node(ToFind, MyId),
    %case K == ToFind of
     %   true -> {ok, K};
      %  false -> findNodes:find_node(Node_Info, ToFind, [K | Nodes] )
    %end.
    %io:display([K | Nodes]),
    findNodes:find_node(Node_Info, ToFind, [K | Nodes] ).

fetch(Node_Info, DataKey) ->
    {MyId, _, _} = Node_Info,
    %[K | Nodes] = kbuckets:find_node(ToFind, MyId),
    Val = kbuckets:find_value(DataKey, MyId),
    case Val of
            {result, Value} -> Value;
            {neighbors, KNodes} -> printList(KNodes), findNodes:find_data(Node_Info, DataKey, KNodes)
    end.
    
%This function is called by the node that recieves a query.
findClosestForRequest(Resquester_Info, Node_Info, ToFind) ->
    {Requester_ID, Requester_IP, Requester_Port} = Resquester_Info,
    {MyId, _, _} = Node_Info,
    kbuckets:checkInsertNewNode(Requester_ID, Requester_IP, Requester_Port, MyId),
    %[K | Nodes] = kbuckets:find_node(ToFind, MyId),
    KNodes = kbuckets:find_node(ToFind, MyId),
    %Sorted = lists:keysort(1,KNodes),
    [X | _] = lists:sort( lists:map(fun({Kid, _, _}) -> Kid bxor ToFind end, KNodes) ),
    Avaliable = (MyId bxor ToFind) < X,
    case Avaliable of
        true ->  unavailable;
        false -> KNodes
    end.

send(Recepient, Message) ->
    {_, RID, _} = Recepient,
    RID ! Message.

loop(Node_Info) ->
    receive
        {find, ToFind} ->
            io:format("Begin finding~n"),
            find(Node_Info, ToFind),
            loop(Node_Info);
        {fetch, DataKey} ->
            fetch(Node_Info, DataKey),
            loop(Node_Info);
        {find_node, Resquester_Info, ToFind} ->
            {ID, _, _} = Node_Info,
            io:format("MyId: ~p, To Find: ~p~n", [ID, ToFind]),
            case ID == ToFind of
                true -> send( Resquester_Info, {node_found, Node_Info});
                false -> Response = findClosestForRequest(Resquester_Info, Node_Info, ToFind),
                         send(Resquester_Info, {notfound, Response})
            end,
            loop(Node_Info);
        {find_data, Resquester_Info, DataKey} ->
            {ID, _, _} = Node_Info,
            Val = kbuckets:find_value(DataKey, ID),
            case Val of
                {result, Value} -> send( Resquester_Info, {data_found, {DataKey, Value}});
                {neighbors, N} -> 
                    [X | _] = lists:sort( lists:map(fun({Kid, _, _}) -> Kid bxor DataKey end, N) ),
                    Avaliable = (ID bxor DataKey) < X,
                    case Avaliable of
                        true -> send(Resquester_Info, {notfound, unavailable});
                        false -> send(Resquester_Info, {notfound, N})
                    end
            end,

            %{ID, _, _} = Node_Info,
            %case ID == DataKey of
             %   true -> send( Resquester_Info, {data_found, Node_Info});
              %  false -> Response = findClosestForRequest(Resquester_Info, Node_Info, DataKey),
               %          send(Resquester_Info, {notfound, Response})
            %end,
            loop(Node_Info)
            %{FoundRes, Response} = findClosestForRequest(Resquester_Info, Node_Info, DataKey),
            %case FoundRes of

    end.
