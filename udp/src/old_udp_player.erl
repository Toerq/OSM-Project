-module(old_udp_player).
-export([init_player/1]).

init_player([Socket, Destination_ip, Destination_port]) ->
    receive_action(Socket, Destination_ip, Destination_port).

receive_action(Socket, Destination_ip, Destination_port) ->
    case gen_udp:recv(Socket, 8, 300) of
	{ok, {Address, Port, Packet}} ->
	    upd_actionhandler:handle_action([Address|Port], Packet, self());
	{error, Reason} -> Reason 
    end,
    receive_and_send_state(Socket, Destination_ip, Destination_port).


receive_and_send_state(Socket, Destination_ip, Destination_port) ->
    receive 
	{state, State} ->
	    gen_udp:send(Socket, Destination_ip, Destination_port, State)
    end.

