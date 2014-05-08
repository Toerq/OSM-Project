-module(erlang_client).

-export([add/2, add/3, remove/1, remove/2, available/0, available/1, clear/0, clear/1]).

add(Server_Name, Ip) -> add(Server_Name, Ip, "localhost").
add(Server_Name, Ip, DestIp) -> simple_rpc({add, Server_Name, Ip}, DestIp).

remove(Server_Name) -> remove(Server_Name, "localhost").
remove(Server_Name, DestIp) -> simple_rpc({remove, Server_Name}, DestIp).

available() -> available("localhost"). 
available(DestIp) -> simple_rpc({available}, DestIp).

clear() -> clear("localhost").
clear(DestIp) -> simple_rpc({clear}, DestIp).

simple_rpc(X, DestIp) ->
    case gen_tcp:connect(DestIp, 3010, 
			 [binary, {packet, 4}]) of
	{ok, Socket} ->
	    gen_tcp:send(Socket, [term_to_binary(X)]),
	    wait_reply(Socket);
	E ->
	    E
    end.

wait_reply(Socket) ->
    receive
 	{tcp, Socket, Bin} ->
 	    Term = binary_to_term(Bin),
	    gen_tcp:close(Socket),
	    Term;
 	{tcp_closed, Socket} ->
	    true
      end.
