-module(game_client).

-export([addPlayer/2, removePlayer/2, getPos/2, getAllPos/1, move/4, wait_reply/1]).

addPlayer(PlayerName, DestIp) -> simple_rpc({addPlayer, PlayerName}, DestIp).
removePlayer(PlayerName, DestIp) -> simple_rpc({removePlayer, PlayerName}, DestIp).
getPos(PlayerName, DestIp) -> simple_rpc({getPos, PlayerName}, DestIp).
getAllPos(DestIp) -> simple_rpc({getAllPos}, DestIp).
move(PlayerName, Direction, Amount, DestIp) -> 
    simple_rpc({move, PlayerName, Direction, Amount}, DestIp).
%%ping(Server_Name, DestIp) -> 
%%    T1 = erlang:now(),
%%    simple_rpc({ping, Server_Name}, DestIp),
%%    timer:now_diff(erlang:now(), T1).
%%clear(DestIp) -> simple_rpc({clear}, DestIp).

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
	    "tcp_is_closed!"
      end.
%%asd: {badrpc,{'EXIT',{undef,[{game_client,getPos,[player1]},{rpc,'-handle_call_call/6-fun-0-',5}]}}}

    

