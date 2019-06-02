%%%-------------------------------------------------------------------
%%% @author daija
%%% @copyright Apache License 2.0, BSD 3, BSD 2, GPL, LGPL, MIT license, Mozilla Public License 2.0, Common Development and Distribution License, Eclipse Public License
%%% @doc
%%%
%%% @end
%%% Created : 24. May 2019 3:12 AM
%%%-------------------------------------------------------------------
-module(kbuckets).

%%NOTE:The exported functions represent the functions provided by this module. However in order to understand these
%%functions calls please go to the API section and read the respective comments for each function.
-export([setUP/0,store/2,find_value/2,checkInsertNewNode/4,find_node/2,ping/3,print/1,hello_world/0,printFindNode/1,printFindValue/1,printAllNodesInTable/0]).

-define(KeySize,5). %%this is the value of the key in bits
-define(BucketSize, 15). %%this is the k-buckets value
-define(MaxBucketSize,?KeySize+1).
-define(Alpha,3).%% it is mentioned in the paper
-define(ThisNodeID,0).%%only used for testing and to generate node data
-define(MaxElems,trunc(math:pow(2,?KeySize))).


getLog(0)->1; %%this wont  be a problem in the real worl use but since we also put the node itself this happens
getLog(Num)->trunc(math:floor(math:log2(Num))) + 1. %%for better understanding with the concept

%%-----------------------data generated to fill the ets table for testing purposes------------------------------------
inputValue([],Index)->Tmp = Index bxor ?ThisNodeID,
  Position = getLog(Tmp),
  ets:insert(buckets,{Position,[{Index,ip,port}]});
inputValue([Head|_],Index)->{Pos,CurrList} = Head,
  inputValueCont(Pos,CurrList,Index,length(CurrList)-?BucketSize).

inputValueCont(_P,_C,_Index,0)->ok; %%do nothing list is full in this case

inputValueCont(Pos,CurrList,Id,Num)->New = [{Id,ip,port}],
  NewList = CurrList++New,
  ets:insert(buckets,{Pos,NewList}).

genRandomData(0)-> Tmp = 0 bxor ?ThisNodeID,
  Position = getLog(Tmp),
  A = ets:lookup(buckets,Position),
  inputValue(A,0);
genRandomData(Num)->Tmp = Num bxor ?ThisNodeID,
  Position = getLog(Tmp),
  A = ets:lookup(buckets,Position),
  inputValue(A,Num),
  genRandomData(Num-1).
%%----------------------------------------End of testing code---------------------------------------------------------

%%-------------------------------Helper functions for the API functions-----------------------------------------------
returnNAboveElemsEmpty([],N,Index,Id)->returnNAboveElems(N, Index + 1, Id);
returnNAboveElemsEmpty(A,N,Index,Id)->
  [Head|_] = A,
  {Position,CurrList} = Head,
  CurrElemsToComp = [{NodeID,Ip,Port,NodeID bxor Id} || {NodeID,Ip,Port} <- CurrList],
  if
    length(CurrList) >= N ->
      CurrElemsToComp;
    true -> %%if we need more elements
      B = returnNAboveElems(N - length(CurrElemsToComp), Index + 1, Id),
      CurrElemsToComp ++ B
  end.

%%Dont forget to CHANGE MAX BUCKET SIZE + 1 HERE FOR OTHER IMPLEMENTATIONS
returnNAboveElems(N,?MaxBucketSize,OurId)->[]; %%if we run out of buckets to search for
returnNAboveElems(N,Index, Id)->A = ets:lookup(buckets,Index),
  returnNAboveElemsEmpty(A,N,Index,Id).

returnNBelowElemsEmpty([],N,Index,Id)->returnNBelowElems(N, Index - 1, Id);
returnNBelowElemsEmpty(A,N,Index,Id)->
  [Head|_] = A,
  {Position,CurrList} = Head,
  CurrElemsToComp = [{NodeID,Ip,Port,NodeID bxor Id} || {NodeID,Ip,Port} <- CurrList],
  if
    length(CurrList) >= N ->
      CurrElemsToComp;
    true -> %%if we need more elements
      B = returnNBelowElems(N - length(CurrElemsToComp), Index - 1, Id),
      CurrElemsToComp ++ B
  end.

returnNBelowElems(N,0,Id)->[]; %%if we run out of buckets to search for
returnNBelowElems(N,Index,Id)->A = ets:lookup(buckets,Index),
  returnNBelowElemsEmpty(A,N,Index,Id).


%%this function seperates the target node from the list the results is {Target node details, The other part of the list}
%% Careful target node returned is a list with one object or empty, the other part is the list then
separateTargetNode(TargetId,List)->
  Target = [{NodeID,IP,Port} || {NodeID,IP,Port} <- List, NodeID == TargetId],
  Other = [{NodeID,IP,Port} || {NodeID,IP,Port} <- List, NodeID /= TargetId],
  {Target,Other}.

%%TODO This should be modified as for the moment it is just a place holder
ping_helper(NodeId,IP,Port)->alive.

pingAction(alive,Head,Tail,NodeID,Ip,Port)->
  Tail++[Head]; %%Since head is alive it is most recently seen
pingAction(dead,Head,Tail,NodeID,Ip,Port)->
  Tail++[{NodeID,Ip,Port}].

checkIfAlive([],NodeID,Ip,Port,OtherList)->
  [Head|Tail] = OtherList,
  {HeadID,HeadIp,HeadPort} = Head,
  PingRes = ping_helper(HeadID,HeadIp,HeadPort),
  pingAction(PingRes,Head,Tail,NodeID,Ip,Port);

checkIfAlive([Head|_],NodeID,Ip,Port,OtherList)->
  OtherList++Head. %%since we received a request from this node we do not need to ping it again.

%%this function checks if the node sending the request is in the k-buckets then it is simply moved to the back,
%% if not it pings the head, if the head is alive then it puts head to the end of the list and discards the new node
%%however if the head is not alive then it removes the head and puts the new node to the back as the most recently seen one
checkIfPlaceForNode(NodeID,List,IP,Port,Position)->
  {Target,Other} = separateTargetNode(NodeID,List),
  AliveRes = checkIfAlive(Target,NodeID,IP,Port,Other),
  ets:insert(buckets,{Position,AliveRes}).

findKClosestBucketsCont(A,Id,0,Num)->A;
findKClosestBucketsCont(A,Id,Size,Index) ->
  Up = returnNAboveElems(Size,Index + 1,Id),
  Do = returnNBelowElems(Size, Index - 1, Id),
  Tmp = Up++Do,
  Res = lists:sublist(lists:keysort(4,Tmp),Size),
  ResCleaned = [{NodeID,Ip,Port}||{NodeID,Ip,Port,Distance}<-Res],
  A++ResCleaned. %%These are the k closest elements

%%This function solves the problem of having no elements for that bucket where by we would get an error trying to get
%%the [Head|_] from A in the end. This bug was present in the findKClosestBuckets.
findKClosestBucketsBugFix([],IdToSearch,Position)->
  findKClosestBucketsCont([],IdToSearch,?BucketSize,Position);
findKClosestBucketsBugFix(A,IdToSearch,Position)->
  [Head|_] = A,
  {Pos,List} = Head,
  findKClosestBucketsCont(List,IdToSearch,?BucketSize - length(List),Position).

findKClosestBuckets(IdToSearch,MyID)->
  Tmp = IdToSearch bxor MyID,
  Position = getLog(Tmp),
  A = ets:lookup(buckets,Position),
  findKClosestBucketsBugFix(A,IdToSearch,Position).

%%if we did not find the key in our table then we look for the K closest buckets by using the same algorithm as
%% find_node with the IdToSearch being the Key
find_value_helper([],Key,MyID)->
  {neighbors, findKClosestBuckets(Key,MyID)};
find_value_helper(A,Key,MyID)->%%if we have it then we simply return the value
  [Head|_] = A,
  {K,Value} = Head,
  {result, Value}.

checkInsertNewNodeHelper([],NodeID,Ip,Port,Position)->%%we have an empty bucket for this Id so we just input it
  ets:insert(buckets,{Position,[{NodeID,Ip,Port}]});
checkInsertNewNodeHelper(A,NodeID,Ip,Port,Position)->
  [Head|_] = A,
  {Pos,List} = Head,
  case length(List) == ?BucketSize of
    true -> checkIfPlaceForNode(NodeID,List,Ip,Port,Position);
    false -> ets:insert(buckets,{Position,List++[{NodeID,Ip,Port}]})
  end.
%%---------------------------------------------End of API helper functions--------------------------------------------

%% ----------------------------------------------------------API------------------------------------------------------
setUP()->%%NOTE: This function should be called first as it sets up the ets tables
  ets:new(buckets,[set, private ,named_table]),
  ets:new(files,[set, private ,named_table]).

store(Key,Value)->%%this function will store a key value pair
  ets:insert(files,{Key,Value}).

%%This function checks to see if we have the value, if we do we return the value, otherwise we return a list of
%% {NodeID, IP, Port}  closest to the key to query for the value.
find_value(Key,MyID)->
  A = ets:lookup(files,Key),
  find_value_helper(A,Key,MyID).

%%NOTE: This function should be called before any received RPC as it checks if we can save the node,
%%as such the details of the RPC node should be provided.
%%This function takes the new node and checks to see if it can be inserted in the appropriate bucket
%%This function can also be used to store new nodes in general however if there is no place then it
%%ping the least recently seen node and then proceed accordingly
checkInsertNewNode(NodeID,Port,IP,MyId)->
  Tmp = NodeID bxor MyId,
  Position = getLog(Tmp),
  A = ets:lookup(buckets,Position),
  checkInsertNewNodeHelper(A,NodeID,IP,Port,Position).

%%NOTE: This function does not do any recursive calls to other nodes, it only returns the k closest nodes
%%This function returns the K-closest nodes to the NodeID, the current ID should be passed as well
find_node(NodeID,MyId)->
  findKClosestBuckets(NodeID,MyId).

%%TODO: ping_helper is not implemented, as such it returns the atom alive, just so that it can be used in testing at least
ping(NodeId,IP,Port)->
  ping_helper(NodeId,IP,Port).

%%This is just for convenience
print(Value)->io:format("\nThe value is: ~p.",[Value]).

printFindValue(Value) ->  print(find_value(Value,?ThisNodeID)).

printFindNode(Value) ->  print(find_value(Value,?ThisNodeID)).

printAllNodesInTable()->print(ets:match_object(buckets, {'$0', '$1'})).
%%------------------------------------------------------------END OF API----------------------------------------------


test()-> io:format("\nThe value is: ~p.",[ets:lookup(ingredients,getLog(15))]).

check1(Num)->genRandomData(Num). %MaxElements -1
check2()->print(ets:match_object(buckets, {'$0', '$1'})).

hello_world()->
%%  ets:new(buckets,[set, private ,named_table]),
%%  ets:insert(buckets,{1,[{1,ip,port}]}),
%%  ets:insert(buckets,{1,[{1,ip,port},{2,ip,port}]}),
%%          genRandomData(?MaxElems-1),
%%  print(findKClosestBuckets(3,?ThisNodeID)),
%%  print(ets:match_object(buckets, {'$0', '$1'})).
  setUP(),
  genRandomData(?MaxElems-1),
  store(5,value),
  store(6,value2),
  print(find_value(7,?ThisNodeID)).