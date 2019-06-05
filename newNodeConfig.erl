-module(newNodeConfig).
-export([bootstrapId/1,initializeNewNode/2] ).


bootstrapId(Pids) ->
    Last = lists:reverse(Pids),
    findAliveBootstrap(Last).

initializeNewNode(Node_ID, Server) -> 
    node:init(Node_ID, self(), Server).

findAliveBootstrap([]) ->
    receive
        {bootstrap, Server} -> 
            NId = spawn(newNodeConfig, initializeNewNode, [22, Server]),
            NId
    end;
findAliveBootstrap([P | Pids]) ->
    P ! {newNodeRequest, self(), 22},
    receive
        {bootstrap, Server} -> 
            NId = spawn(newNodeConfig, initializeNewNode, [22, Server]),
            NId
    after 100 -> findAliveBootstrap(Pids)
    end.