%%%-------------------------------------------------------------------
%%% File    : eb_server.erl
%%% Author  : Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%% Description : The ErlyBank account server.
%%%
%%% Created :  5 Sep 2008 by Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%%-------------------------------------------------------------------
-module(geese_player).

-behaviour(gen_server).

-compile(export_all).
%% API
-export([start_link/1, checkout/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {player_id, accept_socket, table_ref, db_name, state_sender, name}).

-define(SERVER, ?MODULE).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Accept_socket) ->
    gen_server:start(?MODULE, [Accept_socket], []).
						%    gen_server:start({local, ?SERVER}, ?MODULE, [Accept_socket, Server_Pid, Dispatcher_pid], []).

checkout(Who, Book) -> gen_server:call(?MODULE, {checkout, Who, Book}).	

init([Accept_socket]) ->
    %%  Coordinator = gen_server:call(Server_pid, get_state),
						% gen_server:call(Dispatcher_pid, lewut),
						% Pid = proc_lib:spawn_link(fun() -> greet_state(Accept_socket, Dispatcher_pid) end),
						%geese_coordinator:join_lobby(Pid),
						%gen_tcp:controlling_process(Accept_socket, Pid),
    {ok, #state{player_id = self(), accept_socket = Accept_socket, name = "Player"}}.

						%talk_state() -> gen_server:cast(?MODULE, talk_state).


%% @doc Recieves packages sent through the TCP-protocol as messages and handles them in a pre-defined way.
talk_state(State) -> 
    io:format("<In talk state>~n"),
    Accept_socket = State#state.accept_socket,
    Player_id = State#state.player_id,
    receive
	{tcp, Accept_socket, Packet} ->
	    io:format("~nPacket ->~p<-~n",[binary_to_term(Packet)]),
	    case binary_to_term(Packet) of
		{change_name, Name} ->
		    New_state = State#state{name = Name},
		    %% TODO geese_coordinator:change_name(Player_id, Name, Accept_socket),
		    talk_state(New_state);

		add_table_hw ->
		    geese_coordinator:add_table(hw_name, hw_game_type, 5),
		    talk_state(State);
		    
		{add_table, Name, Game_type, Max_players} -> 
		    geese_coordinator:add_table(Name, Game_type, Max_players),
		    talk_state(State);

		debug_print_state ->
		    io:format("~nState: ~p~n", [State]),
		    talk_state(State);

		debug_reset_state ->
		    New_state = State#state{name = "Player", db_name = undef, state_sender = undef, table_ref = undef},
		    talk_state(New_state);

		ping ->
		    gen_tcp:send(Accept_socket, term_to_binary(pong)),
		    talk_state(State);

		my_id ->
		    gen_tcp:send(Accept_socket, term_to_binary(State#state.player_id)),
		    talk_state(State);

		browse_tables ->
		    Tables = geese_coordinator:browse_tables(),
		    String1 = lists:flatten(io_lib:format("~p~n", [Tables])),
		    io:format("~noutput from browse_tables: ~p~n", [Tables]),
		    gen_tcp:send(Accept_socket, term_to_binary(Tables)),
		    talk_state(State);

		browse_players -> 
		    Players = geese_coordinator:browse_players(),
		    String1 = lists:flatten(io_lib:format("~p~n", [Players])),
		    io:format("jaasd"),
		    gen_tcp:send(Accept_socket, term_to_binary(Players)),
		    talk_state(State);

		join_table_debug ->
		    {Table_pid, Game_pid, Db_name} = geese_coordinator:join_table_debug(Player_id, not_used),
		    New_state = State#state{table_ref = Table_pid, db_name = Db_name, state_sender = Game_pid},

		    Name = State#state.name,
		    %% {X,Y} = {random:uniform(500), random:uniform(500)},		    		    
		    io:format("Point1~n"),
		    Call = {action_add, Db_name, server, add_player, [{Name, {15,15}, {0,0}, 100, 50, Player_id}]},
		    game_state:register_action(Call),

		    game_state(New_state);

		{join_table, Table_ref} ->
		    Socket = State#state.accept_socket,
		    case geese_coordinator:join_table(Player_id, Table_ref) of
			{Table_pid, Game_pid, Db_name} ->
			    New_state = State#state{table_ref = Table_pid, db_name = Db_name, state_sender = Game_pid},
			    Name = State#state.name,
			    %% {X,Y} = {random:uniform(500), random:uniform(500)},		    		    
			    io:format("Point1~n"),
			    Call = {action_add, Db_name, server, server, [add_player, {Name, {15,15}, {0.0,0.0}, 100, 50, Player_id}]},
			    game_state:register_action(Call),
			    gen_tcp:send(Socket, term_to_binary(join_succeeded)),
			    game_state(New_state);
			join_failed ->
			    gen_tcp:send(Socket, term_to_binary(join_failed)),
			    talk_state(State)
		    end;

		{join_table_java, Table_ref} ->
		    {Table_pid, _Game_pid, Db_name} = geese_coordinator:join_table(Player_id, Table_ref),
		    talk_state(State);

		join_table_debug ->
		    {Table_pid, _Game_pid, Db_name} = coordinator:join_table(Player_id, not_used),
		    talk_state(State);

		remove_player_from_table ->
		    coordinator:remove_player_from_table(self()),
		    talk_state(State);

		Arbitary -> 
		    io:format("~n In Arbitary-clause, recieved ~p~n", [Arbitary]),
		    talk_state(State)
	    end;
	E -> 
	    io:format("<<<E>>>: ~p vs. Accept_socket ~p", [E, Accept_socket])
    end.

game_state(State) ->
%%    io:format("<In game state>~n"),
    Accept_socket = State#state.accept_socket, 
    Db_name = State#state.db_name,  
    State_sender = State#state.state_sender,
    Table_ref = State#state.table_ref,
    Player_id = State#state.player_id,
    receive 
	{tcp, Accept_socket, Packet} ->
%%	    io:format("<Game command -In->~n term:~w~n", [binary_to_term(Packet)]),
	    case binary_to_term(Packet) of
		{do_action, {Action, Var_list}} ->
		    Call = {action_add, Db_name, Player_id, Action, Var_list},
		    game_state:register_action(Call),
		    game_state(State);

		get_state ->
		    State_sender ! {get_state, Player_id},
		    game_state(State);

		leave_game ->
		    tbi,
		    talk_state(State);

		E ->
		    tbi
 		    %% annat
	    end;
	{state, Game_state} ->
%%	    io:format("<State command -Out->~n"),
	    %% tcp send Game state socket
	    Bin = term_to_binary({state, Game_state}),
	    gen_tcp:send(Accept_socket, Bin),
	    game_state(State);
	E ->
	    tbi
	    %% annat
    end.

handle_cast(start_player, State) ->
    Accept_socket = State#state.accept_socket, 
    Name = State#state.name, 
    Player_id = State#state.player_id,
    geese_coordinator:join_lobby(Player_id, Name, Accept_socket),
    talk_state(State),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}la
%% Description: Handling cast messages
%% gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
%%--------------------------------------------------------------------

handle_call({call}, _From, State) ->
    {reply, State, State}.



%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
