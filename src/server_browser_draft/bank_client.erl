-module(bank_client).

-export([add/2, remove/1, available/0, ping/1]).

add(Server_Name, Ip) -> simple_rpc({add, Server_Name, Ip}).
remove(Server_Name) -> simple_rpc({remove, Server_Name}).
available() -> simple_rpc({available}).
ping(Server_Name) -> simple_rpc({ping, Server_Name}).

simple_rpc(X) ->
    case gen_tcp:connect("localhost", 3010, 
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

    
