-module(udp_loop).

addPlayer(PlayerName, Ip, Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary, {ip, Ip}, inet]),
    spawn(udp_loop, player_handler, [Socket, Ip, Port, PlayerName]),
    



player_handler(Socket, Ip, Port, PlayerName) ->
    receive
	{udp, Socket_r, Ip_r, Port_r, Bin} ->
	    Term = binary_to_term(Bin),
	    do_action(Term, PlayerName),
	    player_handler(Socket, Ip, Port, PlayerName);
	E ->
	    {error, E}
    end.


   
