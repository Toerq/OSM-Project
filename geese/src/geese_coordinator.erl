-module(geese_coordinator).

-behaviour(gen_server).

-export([join_lobby/3, join_table/2, add_table/3, browse_tables/0, browse_players/0, get_state/0, remove_player_from_table/1, remove_table/1, remove_player_from_lobby/1]).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, stop/0]).



-record(coordinator_state,
	{
	  players = [],
	  test_table,
	  tables,
	  test}
       ).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    case geese_coordinator_backup:get_state() of
	no_state_saved ->
	    {ok, #coordinator_state{tables = []}};
	State -> 
	    io:format("~n -------- Restarting coordinator with state: ~p -------~n", [State]),
	    {ok, State}
    end.

join_lobby(Pid, Name, Socket) -> gen_server:call(?MODULE, {join_lobby, Pid, Name, Socket}).
browse_tables() -> gen_server:call(?MODULE, browse_tables).
join_table(Player_id, Table_ref) -> gen_server:call(?MODULE, {join_table, Player_id, Table_ref}).
add_table(Name, Game_type, Max_players) -> gen_server:cast(?MODULE, {add_table, Name, Game_type, Max_players}).
browse_players() -> gen_server:call(?MODULE, browse_players_on_server).
get_state() -> gen_server:call(?MODULE, get_state).
remove_player_from_table(Player_id) -> gen_server:call(?MODULE, {remove_player_from_table, Player_id}).
remove_table(Table_pid) -> gen_server:call(?MODULE, {remove_table, Table_pid}).
remove_player_from_lobby(Player_id) -> gen_server:cast(?MODULE, {remove_player_from_lobby, Player_id}).
    


back_up(State) ->
    geese_coordinator_backup:back_up(State).

handle_call(browse_players_on_server, _From, State) ->
    {reply, State#coordinator_state.players, State};

handle_call(browse_tables, _From, State) ->
    {reply, State#coordinator_state.tables, State};

handle_call({remove_player_from_table, Pid}, _From, State) ->
    Players = State#coordinator_state.players,
    Tables = State#coordinator_state.tables,
    case lists:keyfind(Pid, 1, Players) of
	{_, _, _, not_in_any_table} ->
	    {reply, not_in_any_table, State};
	{_, _, _, Table_ref} ->
	    {_, Table_name, Game_type, Connected_players, Max_players} = lists:keyfind(Table_ref, 1, Tables),
	    New_table = {Table_ref, Table_name, Game_type, Connected_players - 1, Max_players},
	    New_table_list = lists:keydelete(Table_ref, 1, Tables),
	    New_state = State#coordinator_state{tables = [New_table | New_table_list]},
	    Reply = gen_server:call(Table_ref, {remove_player, Pid}),
	    back_up(New_state),
	    {reply, Reply, New_state}
    end;

handle_call({join_table, Pid, Table_ref}, _From, State) ->
    Tables = State#coordinator_state.tables,
    Players = State#coordinator_state.players,
    case lists:keyfind(Pid, 1, Players) of
	false ->
	    {reply, spelare_finns_ej_i_join_table_coordinator, State};
	{Pid, Player_name, Socket, _} ->
	    case gen_server:call(Table_ref, {join_table, Pid, Player_name, Socket}) of
		join_failed ->
		    {reply, join_failed, State};
		Tuple ->
		    case lists:keyfind(Table_ref, 1, Tables) of
			false -> 
			    {reply, table_not_found, State};
			{_Table_ref, _Table_name, _Game_type, Connected_players, _Max_players} ->
			    %% add succeeded
			    NewTable = {_Table_ref, _Table_name, _Game_type, Connected_players + 1, _Max_players},
			    New_player_list = lists:keydelete(Pid, 1, Players),
			    New_player = {Pid, Player_name, Socket, Table_ref}, 
			    New_table_list = lists:keydelete(Table_ref, 1, Tables),
			    New_state = State#coordinator_state{players = [New_player | New_player_list], tables = [NewTable | New_table_list]},
			    back_up(New_state),
			    {reply, Tuple, New_state}	
		    end
	    end
    end;

handle_call({join_lobby, Pid, Name, Socket}, _From, State) ->
    Players = State#coordinator_state.players,
    case lists:keyfind(Pid, 1, Players) of
	false ->
	    New_state = State#coordinator_state{players = [{Pid, Name, Socket, not_in_any_table} | Players]},
	    back_up(New_state),
	    {reply, join_succeeded, New_state};
	_Player -> 
	    {reply, player_already_exists, State}
    end;

handle_call({remove_table, Table_pid}, _From, State) ->
    Tables = State#coordinator_state.tables,
    case lists:keyfind(Table_pid, 1, Tables) of
	false ->
	    {reply, no_such_table_exists, State};
	_Table -> 
	    NewTables = lists:keydelete(Table_pid, 1, Tables),
	    New_state = State#coordinator_state{tables = NewTables},
	    gen_server:cast(Table_pid, exit),
	    back_up(New_state),
	    {reply, removal_succeeded, New_state}
    end;

handle_call(get_state, _From, State) ->
    {reply, State, State}.

handle_cast({add_table, Name, Game_type, Max_players}, State) ->
    {ok, Table_pid} = geese_table:start_link(Max_players),
    io:format("~ntable pid from add_table: ~p~n", [Table_pid]),
    {Players, Nmbr, Maxplyrs} = gen_server:call(Table_pid, get_state),
    io:format("~nPlayers: ~p, NmbrOfPlayers: ~p, Maxplayers: ~p~n", [Players, Nmbr, Maxplyrs]),
    Tables = State#coordinator_state.tables,
    Table = {Table_pid, Name, Game_type, 0, Max_players},
    Tables =  State#coordinator_state.tables,
    New_state = State#coordinator_state{tables = [Table | Tables], test_table = Table_pid},
    back_up(New_state),
    {noreply, New_state};

handle_cast({remove_player_from_lobby, Player_id}, State) ->
    Players = State#coordinator_state.players,    
    case lists:keydelete(Player_id, 1, Players) of
	false ->
	    New_state = State;

	New_player_list ->
	    New_state = State#coordinator_state{players = New_player_list}
    end,
    {noreply, New_state}.


handle_info(_Info, State) ->
    {noreply, State}.

stop() ->
    gen_server:cast(?MODULE, stop).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

