%% SOURCE:
%% http://erlycoder.com/25/erlang-tcp-client-server-sockets-with-gentcp
%%

%% ERLANG ECHO-TCP-SERVER:
%% 1> c(tcp_echo).
%% {ok,tcp_echo}
%% 2> tcp_echo:listen(4020).
%% <0.38.0>
%%
%% 
%% JAVA TCP CLIENT:
%% > javac ClientSocket.java
%% > java ClientSocket
%%
%% Connecting to server on port 4020
%% Just connected to /127.0.0.1:4020
%% Client received: Hello from /127.0.0.1:54215 from Server
%% 

-module(tcp_echo).
-export([listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, true}, {reuseaddr, true}]).
-define(PORT, 4020).

% Call echo:listen() to start the server.
listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    spawn(fun() -> accept(LSocket) end).

% Wait for incoming connections and spawn a process that will process incoming packets.
accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    Pid = spawn(fun() ->
			io:format("Connection accepted ~n", []),
			loop(Socket)
		end),
    gen_tcp:controlling_process(Socket, Pid),
    accept(LSocket).

% Echo back whatever data we receive on Socket.
loop(Sock) ->
    %inet:setopts(Sock, [{active, once}]),
    receive
	{tcp, Socket, Data} ->
	    io:format("Got packet: ~p~n", [binary_to_term(Data)]),
	    
	    %% Send 4 bytes (32 bits) first to declare the size
	    %% Java doesn't handle unsigned ints, so 8 bits
	    %% range between -127 and 127
	    %% Bin = <<4:32,1:8, 128:8, 123:8, 45:8>>,
	    gen_tcp:send(Socket, term_to_binary(pong)),
	    loop(Socket);
	{tcp_closed, Socket}->
	    io:format("Socket ~p closed~n", [Socket]);
	{tcp_error, Socket, Reason} ->
	    io:format("Error on socket ~p reason: ~p~n", [Socket, Reason])
    end.
