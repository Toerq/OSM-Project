-module(udp_test).
-export([server/1, client/1, client/3, startclient/0]).
% source: http://blog.xrdavies.com/?p=170 
% First Erlang UDP server/client
% Author: Rui Xie
% Date: 2012-06-06
% Add concurrent processing.
% Define the interface to create server side.
server(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    loop(Socket).

% Define the server side behavior.
loop(Socket) ->
    receive
	{udp, Socket, Host, Port, Bin} ->
	    gen_udp:send(Socket, Host, Port, Bin),
	    io:format("~s~n", [Bin]),
	    loop(Socket)
    end.

% Here server receives message prints it and sends back a response 
% using the exactly the same message from client side. 
% Then loop for next message.
% Define interface to create client side.
% This one client/1 is used for test. Simple one.
client(Request) ->
    {ok, Socket} = gen_udp:open(0,[binary]),
    ok = gen_udp:send(Socket, "135.251.143.229", 53530, Request),
    Value = receive
		{udp, Socket, _, _, Bin} ->
		    {ok, Bin}
	    after 2000 ->
		    error
	    end,
    gen_udp:close(Socket),
    Value.

% Define client/3
client(Host, Port, Request) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    Id = 1,
    loopclient(Socket, Host, Port, Request, Id + 1).

% Define that the client will send message for 400 times
loopclient(Socket, Host, Port, Request1, Id) when Id < 400 ->
    ok = gen_udp:send(Socket, Host, Port, Request1),
    loopclient(Socket, Host, Port, Request1, Id + 1);
loopclient(Socket, Host, Port, Request1, Id) ->
    gen_udp:close(Socket),
    done.

% Define interface to start several clients at the same time by 
% using spawn. Here I defined 14 processes to simulate 14 concurrent 
% clients. Each one will send 400 UDP packages.
startclient() ->
     spawn(udp_test, client, ["localhost", 53530, "hello world from erlang client 1"]).

%% ADD MORE CLIENTS:

     %spawn(udp_test, client, ["localhost", 53530, "hello world from erlang client 2"]).
     %spawn(udp_test, client, ["localhost", 53530, "hello world from erlang client 3"]),
     %spawn(udp_test, client, ["localhost", 53530, "hello world from erlang client 4"]),
     %spawn(udp_test, client, ["localhost", 53530, "hello world from erlang client 5"]),
     %spawn(udp_test, client, ["localhost", 53530, "hello world from erlang client 6"]),
     %spawn(udp_test, client, ["localhost", 53530, "hello world from erlang client 7"]),
     %spawn(udp_test, client, ["localhost", 53530, "hello world from erlang client 8"]),
     %spawn(udp_test, client, ["localhost", 53530, "hello world from erlang client 9"]),
     %spawn(udp_test, client, ["localhost", 53530, "hello world from erlang client 10"]),
     %spawn(udp_test, client, ["localhost", 53530, "hello world from erlang client 11"]),
     %spawn(udp_test, client, ["localhost", 53530, "hello world from erlang client 12"]),
     %spawn(udp_test, client, ["localhost", 53530, "hello world from erlang client 13"]),
     %spawn(udp_test, client, ["localhost", 53530, "hello world from erlang client 14"]).
