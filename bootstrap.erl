-module(bootstrap).
-export([bootstrap/0]).

%%NOTE: this part is called with bootsrtap:bootstrap() and everything then works

-define(OurID,0). % the bootstrap nodes id

printList([]) -> io:format("~n");
printList( [L|List]) ->
    io:format("~p ", [L]),
    printList(List).

toatom([ID | Rest]) ->
    list_to_atom(ID ++ Rest).

startShit([], _) -> done;
startShit(A, open)->
    [Head|Tail] = A,
    {NodeID,IP,Port} = Head,
    open_port({spawn, "werl -sname " ++ IP}, []), %calls a new erlang window  <----------- This needs to be changed from werl to erl for linux 
    startShit(Tail, open);

startShit(A, spawn)->
    [Head|Tail] = A,
    {NodeID,IP,Port} = Head,
    NewPort = spawn(toatom(IP), node, init, [NodeID, toatom(IP), self()]),
    %kbuckets:checkInsertNewNode(NodeID,IP,NewPort,?OurID),

    %IP and Pid are switched from original (look above).
    kbuckets:checkInsertNewNode(NodeID,NewPort,IP,?OurID),
    startShit(Tail, spawn).

%%this adds the newly created elements to the bootstraps nodes k-buckets
addElems(A)->
    startShit(A, open),
    timer:sleep(1000),
    startShit(A, spawn).


%this function handles new requests from newly created nodes
sendKBuckets() ->
    receive
        {kbucket, Pid, NodeID} ->
            %io:format("~p requested k bucket~n", [Pid]),
            Res = kbuckets:find_node(NodeID,?OurID),
            %io:format("To: ~p~n", [NodeID]),
            %printList(Res),
            Pid ! {kresp, Res},
            sendKBuckets();
        {newNodeRequest, Pid, Node_Id} ->
            % Send Node_Id from newNodeConfig
            % Return K bucket to node.
            % Test with finding nodes.
            %Kb = kbuckets:find_nodes(Node_Id, ?OurID),
            %Pid ! {bootstrap, Kb}
            io:format("~p sent bootstrap request~n", [Pid]),
            Pid ! {bootstrap, self()},
            sendKBuckets()
    end.

%This is the bootstrap function, by calling this function you create 20 nodes with ID 1..20 @MSI, The hash function is commented out 
bootstrap()->
    kbuckets:setUP(),
    IDs = lists:seq(1,20),
    Nodes = [{NodeID,string:concat(io_lib:format("~p", [NodeID]),"@MSI"),69} || NodeID <- IDs], %%crypto:bytes_to_integer(crypto:hash(md5,lists:flatten(io_lib:format("~p", [NodeID])))), NodeID, 69} || NodeID <- IDs],
    addElems(Nodes),
    kbuckets:print("allElemesAdded"),
    kbuckets:printAllNodesInTable(),
    sendKBuckets().

% send(Msg, IP) ->
%     IP ! Msg.

% requests()->
%     receive
%          {giveBucket, Node_Info} ->
%             {NodeID, IP, _} = Node_Info,
%             A = hello:find_node(NodeID,?OurID),
%             B = [{ID,Ip,Port} || {ID,Ip,Port} <- A, NodeID /= ID],
%             send(B, IP)
%     end.