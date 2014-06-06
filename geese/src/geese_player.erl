%% @author Christian Törnqvist
-module(geese_player).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, talk_state/1, game_state/1,
         terminate/2, code_change/3]).

-record(state, {player_id, accept_socket, table_ref, db_name, state_sender, name}).

start_link(Accept_socket) ->
    gen_server:start(?MODULE, [Accept_socket], []).

init([Accept_socket]) ->
    {ok, #state{player_id = self(), accept_socket = Accept_socket, name = "Player"}}.

%% @doc Communicates with the client through TCP connection. Recieves packages, and the handles them in a predefined way. The following are the defined receive terms:
%% === {get_state, Name} ===
%% <div class="example">
%% Changes the name of the player stored in the State of the player process.
%% </div> 
%% === {add_table, Name, Game_type, Max_players} ===
%% <div class="example">
%% Calls the coordinator module and adds a new table  with name Name, game_type Game_type, and max players Max_players.
%% </div>
%% === ping ===
%% <div class="example">
%% Sends the erlang atom pong through TCP.
%% </div>
%% === browse_tables ===
%% <div class="example">
%% Sends the list of tables stored in the coordinator module through TCP.
%% </div>
%% === browse_players ===
%% <div class="example">
%% Sends the list of players stored in the coordinator module through TCP. 
%% </div>
%% === {join_table, Table_ref} ===
%% <div class="example">
%% Attempts to join a new table with the current player process and table with reference Table_ref by calling geese_coordinator:join_table/2.
%% Either returns the atom join_succeeded or join_failed depending on wether the join succeeded or not.
%% </div>
%% === leave_game ===
%% <div class="example">
%% Removes the player element of the called player process in the coordinator module. Also terminates the player process being called.
%% </div>
talk_state(State) -> 
    io:format("<In talk state>~n"),
    Accept_socket = State#state.accept_socket,
    Player_pid = State#state.player_id,
    receive
	{tcp, Accept_socket, Packet} ->
	    io:format("~nPacket ->~p<-~n",[binary_to_term(Packet)]),
	    case binary_to_term(Packet) of
		{change_name, Name} ->
		    New_state = State#state{name = Name},
		    talk_state(New_state);
		    
		{add_table, Name, Game_type, Max_players} -> 
		    geese_coordinator:add_table(Name, Game_type, Max_players),
		    talk_state(State);

		ping ->
		    gen_tcp:send(Accept_socket, term_to_binary(pong)),
		    talk_state(State);

		my_id ->
		    gen_tcp:send(Accept_socket, term_to_binary(State#state.player_id)),
		    talk_state(State);

		browse_tables ->
		    Tables = geese_coordinator:browse_tables(),
		    gen_tcp:send(Accept_socket, term_to_binary(Tables)),
		    talk_state(State);

		browse_players -> 
		    Players = geese_coordinator:browse_players(),
		    gen_tcp:send(Accept_socket, term_to_binary(Players)),
		    talk_state(State);

		{join_table, Table_ref} ->
		    Socket = State#state.accept_socket,
		    case geese_coordinator:join_table(Player_pid, Table_ref) of
			{Table_pid, Game_pid, Db_name} ->
			    New_state = State#state{table_ref = Table_pid, db_name = Db_name, state_sender = Game_pid},
			    Name = State#state.name,
			    io:format("Point1~n"),
			    Call = {action_add, Db_name, server, server, [add_player, {Name, {15,15}, {0.0,0.0}, 100, 50, {0,0,0}, Player_pid}]},
			    game_state:register_action(Call),
			    gen_tcp:send(Socket, term_to_binary(join_succeeded)),
			    game_state(New_state);
			join_failed ->
			    gen_tcp:send(Socket, term_to_binary(join_failed)),
			    talk_state(State)
		    end;

		leave_game ->
		    geese_coordinator:remove_player_from_lobby(self()),
		    exit('Client left');
		    
		Arbitary -> 
		    io:format("~n In Arbitary-clause, recieved ~p~n", [Arbitary]),
		    talk_state(State)
	    end;
	E -> 
	    io:format("<<<E>>>: ~p vs. Accept_socket ~p", [E, Accept_socket])
    end.

%% @doc The function that the client communicates with when inside a game. The game_state module also communicates with this module.
%% === {do_action, {Action, Var_list}} (received through TCP) ===
%% <div class="example">
%% Calls game_state:register_action/1 with the received tuple as argument. 
%% </div>
%% === get_state (received through TCP) ===
%% <div class="example">
%% Sends the current state stored in the module game_state through TCP
%% </div>
%% === leave_game  (received through TCP) ===
%% <div class="example">
%% Calls game_state:register_action/1 with the received atom as argument. Also calls geese_coordinator:remove_player_from_lobby/1 with its own pid as argument.
%% </div>
%% === {state, Game_state} (received from game_state module) ===
%% <div class="example">
%% Receives the state of the game_state module and sends it through TCP to the client.
%% </div>
game_state(State) ->
    Accept_socket = State#state.accept_socket, 
    Db_name = State#state.db_name,  
    State_sender = State#state.state_sender,
    Player_pid = State#state.player_id,
    receive 
	{tcp, Accept_socket, Packet} ->
	    case binary_to_term(Packet) of
		{do_action, {Action, Var_list}} ->
		    Call = {action_add, Db_name, Player_pid, Action, Var_list},
		    game_state:register_action(Call),
		    game_state(State);

		get_state ->
		    State_sender ! {get_state, Player_pid},
		    game_state(State);

		leave_game ->
		    Call = {action_add, Db_name, Player_pid, server, [remove_player, arg]},
		    game_state:register_action(Call),
		    geese_coordinator:remove_player_from_table(self()),
		    talk_state(State);
		_ ->
		    tbi
	    end;
	{state, Game_state} ->
	    Bin = term_to_binary({state, Game_state}),
	    gen_tcp:send(Accept_socket, Bin),
	    game_state(State);
	_ ->
	    tbi
    end.

%% @doc asynchronous call-handlers.
%% === Case {get_state} ===
%% <div class="example">
%% Calls geese_coordinator:join_lobby/2 with the arguments Player_pid, Name and Accept_socket which are stored in the player process' state.
%% </div>

handle_cast(start_player, State) ->
    Accept_socket = State#state.accept_socket, 
    Name = State#state.name, 
    Player_pid = State#state.player_id,
    geese_coordinator:join_lobby(Player_pid, Name, Accept_socket),
    talk_state(State),
    {noreply, State}.

handle_call({call}, _From, State) ->
    {reply, State, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
