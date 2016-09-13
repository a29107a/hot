# hot
erlang game server frame

# how to compile
rebar get-deps  
rebar compile

# how to start
bash ./sh/test_node/server param  
the param maybe :  
1.start  
    start server no console  
2.live  
    start server have console  
3.stop  
    stop server when server start no console  
4.backup  
    backup server  
5.reload  
    reload the server  
6.debug  
    debug server when server start no console  
7.other  
    tell you how to use   

so,when you want start server,you can :  
bash ./sh/test_node/server start

however,you can also copy several test_node to start mutli server.  
the dir in sh/ maybe is test_node1,test_node2...
