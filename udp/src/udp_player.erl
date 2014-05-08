%%% @doc One line blurb.
%%%
%%% More detailed, multi-line description.
%%% @author JSmith <john.smith@gmail.com>
-module(udp_player).
-export([init_player/1]).

-record(state, {socket, port, destination_ip, destination_port}).

init_player([Socket, Port, DestinationIp, DestinationPort]) ->
    {ok, #state{socket = Socket, port = Port, destination_ip = DestinationIp, destination_port = DestinationPort}, 0},
   receive_action().

receive_action() ->
    case gen_udp:recv(socket, 8, 300) of
	{ok, {Address, Port, Packet}} ->
	   upd_actionhandler([Address|Port], Packet, self());
	{error, reason} -> {error, reason}
	end,
receive_and_send_state().


receive_and_send_state() ->
    receive 
	{state, State} ->
	    gen_udp:send(socket, destination_ip, destination_port, State)
    end.
			 



