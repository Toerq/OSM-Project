-module(server).
-export([start/0, client/1, test/0]).

start() ->
    mnesia:start(),
    spawn(fun() -> server(4000) end).

server(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary, {active, false}]),
    io:format("server opened socket:~p~n",[Socket]),
    loop(Socket).

test() ->
    io:format("Available to binary: ~p~n", [term_to_binary({available})]).

loop(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
	{udp, Socket, Host, Port, Bin} ->
	    %%io:format(binary_to_term(Bin)),
	    io:format("server received:~p~n",[Bin]),
	    Term = binary_to_term(Bin),
	    %%io:format(Term),
	    Reply = do_call(Term),
	    gen_udp:send(Socket, Host, Port, term_to_binary(Reply)),
	    loop(Socket)
    end.

						% Client code
client(N) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    io:format("client opened socket=~p~n",[Socket]),
    ok = gen_udp:send(Socket, "localhost", 4000, N),
    Value = receive
		{udp, Socket, _, _, Bin} ->
		    io:format("client received:~p~n",[Bin])
	    after 2000 ->
		    0
	    end,
    gen_udp:close(Socket),
        Value.


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
