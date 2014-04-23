-module(bank_client).

-export([add/3, remove/2, available/1, ping/2]).

add(Server_Name, Ip, DestIp) -> simple_rpc({add, Server_Name, Ip}, DestIp).
remove(Server_Name, DestIp) -> simple_rpc({remove, Server_Name}, DestIp).
available(DestIp) -> simple_rpc({available}, DestIp).
ping(Server_Name, DestIp) -> 
    T1 = erlang:now(),
    simple_rpc({ping, Server_Name}, DestIp),
    timer:now_diff(erlang:now(), T1).

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

    
