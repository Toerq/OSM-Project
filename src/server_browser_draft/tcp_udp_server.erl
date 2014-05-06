-module(tcp_udp_server).

-export([start/0, stop/0]).

start() ->
    mnesia:start(),
    spawn_link(fun() -> server(3010) end).

stop() ->
    mnesia:stop(),
    tcp_server:stop(3010).

server(Port) ->
    tcp_server:
	start_raw_server(Port, fun(Socket) -> input_handler(Socket) end, 15, 4).

input_handler(Socket) ->
    receive
 	{tcp, Socket, Bin} ->
 	    Term = binary_to_term(Bin),
	    Reply = do_call(Term),
 	    send_term(Socket, Reply),
	    input_handler(Socket);
 	{tcp_closed, Socket} ->
	    true
     end.

game_state(Tick, StateSender) ->
    Time = erlang:now(),
    CurState = mnesia:foldl(fun(X,XS) -> [X|XS] end, [], player),
    CurActions = mnesia:foldl(fun(X,XS) -> [X|XS] end, [], actions),
    NewState = doActions(CurState, CurActions),
    StateSender ! {new_state, NewState},
    SleepTime = (1000 / Tick) - timer:now_diff(erlang:now(), Time),    
    if SleepTime > 0 ->
	    timer:sleep(SleepTime)
    end,
    game_state(Tick, StateSender).











send_term(Socket, Term) ->
    gen_tcp:send(Socket, [term_to_binary(Term)]).

do_call(C) ->
    Fun = the_func(C),
    mnesia:transaction(Fun).

the_func({add_player, PlayerName, Ip}) -> udp_loop:addPlayer(PlayerName, Ip);



the_func({add, Server_Name, Ip})  ->  bank:add(Server_Name, Ip);
the_func({remove, Server_Name}) ->  bank:remove(Server_Name);
the_func({available}) ->  bank:available();
the_func({clear}) ->  bank:clear();
the_func({ping, Server_Name}) -> bank:ping(Server_Name);
the_func({addPlayer, PlayerName}) -> game_logic:addPlayer(PlayerName);
the_func({getPos, PlayerName}) -> game_logic:getPos(PlayerName);
the_func({getAllPos}) -> game_logic:getAllPos();
the_func({move, PlayerName, Direction, Amount}) -> game_logic:move(PlayerName, Direction, Amount).


