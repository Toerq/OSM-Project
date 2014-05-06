-module(bank_server).

-export([start/0, stop/0]).

start() ->
    mnesia:start(),
    spawn_link(fun() -> server(3010) end).

stop() ->
    mnesia:stop(),
    tcp_server:stop(3010).

server(Port) ->
    tcp_server:
	start_raw_server(Port, 
			 fun(Socket) -> 
				 input_handler(Socket) 
			 end, 
			 15,
			 4).

input_handler(Socket) ->
    receive
 	{tcp, Socket, Bin} ->
 	    Term = binary_to_term(Bin),
	    %% io:format("~p", [Term]),
	    Reply = do_call(Term),
 	    send_term(Socket, Reply),
	    input_handler(Socket);
 	{tcp_closed, Socket} ->
	    true
     end.

send_term(Socket, Term) ->
    gen_tcp:send(Socket, [term_to_binary(Term)]).

do_call(C) ->
    Fun = the_func(C),
    mnesia:transaction(Fun).

the_func({add, Server_Name, Ip})  ->  bank:add(Server_Name, Ip);
the_func({remove, Server_Name}) ->  bank:remove(Server_Name);
the_func({available}) ->  bank:available();
the_func({clear}) ->  bank:clear();
the_func({ping, Server_Name}) -> bank:ping(Server_Name);
the_func({addPlayer, PlayerName}) -> game_logic:addPlayer(PlayerName);
the_func({removePlayer, PlayerName}) -> game_logic:removePlayer(PlayerName);
the_func({getPos, PlayerName}) -> game_logic:getPos(PlayerName);
the_func({getAllPos}) -> game_logic:getAllPos();
the_func({move, PlayerName, Direction, Amount}) -> game_logic:move(PlayerName, Direction, Amount).
