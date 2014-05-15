%%%-------------------------------------------------------------------
%%% File    : eb_server.erl
%%% Author  : Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%% Description : The ErlyBank account server.
%%%
%%% Created :  5 Sep 2008 by Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%%-------------------------------------------------------------------
-module(geese_coordinator).

-behaviour(gen_server).

%% API
-compile(export_all).
-export([start_link/0, checkout/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, test/0]).

-record(coordinator_state,
	{%% player = {Pid, Name, Socket}
	  players = [],
	 %%tables = [{table_pid, table_name, game_type, connected_players, max_players}
	 tables = [],
	 test}
).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
checkout(Who, Book) -> gen_server:call(?MODULE, {checkout, Who, Book}).	
join_lobby(Pid, Name, Socket) -> gen_server:call(?MODULE, {join_lobby, Pid, Name, Socket}).
browse_tables() -> gen_server:call(?MODULE, browse_tables).
join_table(Player_id, Table_ref) -> gen_server:call(?MODULE, {join_table, Player_id, Table_ref}).
add_table() -> gen_server:cast(?MODULE, add_table).
browse_players() -> gen_server:call(?MODULE, browse_players).

init([]) ->
  {ok, #coordinator_state{}}.


handle_call(browse_players_on_server, _From, State) ->
   {reply, State#coordinator_state.players, State};

handle_call(browse_tables, _From, State) ->
        {reply, State#coordinator_state.tables, State};

handle_call({join_table, Pid, Table_ref}, _From, State) ->
    Tables = State#coordinator_state.tables,
    Players = State#coordinator_state.players,
    case lists:keyfind(Pid, 1, Players) of
	false ->
	    {reply, spelare_finns_ej_i_join_table_coordinator, State};
	{Pid, Player_name, Socket} ->
	   case gen_server:call(Table_ref, {Pid, Player_name, Socket}) of
	       add_failed ->
		   {reply, add_failed, State};
	       ok ->
		   case lists:keyfind(Table_ref, 1, Tables) of
		       false -> 
			   {reply, table_not_found_obscure, State};
		       {_Table_pid, _Table_name, _Game_type, Connected_players, _Max_players} ->
			   NewTable = {_Table_pid, _Table_name, _Game_type, Connected_players + 1, _Max_players},
			   NewTableList = lists:keydelete(Table_ref, 1, Tables),
			   NewState = State#coordinator_state{tables = [NewTable | NewTableList]},
			   {reply, add_succeeded, NewState}
		   end
	   end
    end;

handle_call({join_lobby, Name, Pid, Socket}, _From, State) ->
    Players = State#coordinator_state.players,
    case lists:keyfind(Pid, 1, Players) of
	%%Spelaren fanns inte i listan
	false ->
	    NewState = State#coordinator_state{players = [{Pid, Name, Socket} | Players]},
	    {reply, join_succeeded, NewState};
	_Player -> 
	    {reply, player_already_exists, State}
    end.
	    

test() ->
    io:format("~nprint från coordinator~p~n", [self()]).

%%tables = [{table_pid, table_name, game_type, connected_players, max_players}
handle_cast(add_table, State) ->
    {ok, Table_pid} = geese_table:start_link(),
    Tables = State#coordinator_state.tables,
    Table = {Table_pid, name, mmo_tetris, 0, 20},
    Tables =  State#coordinator_state.tables,
    NewState = State#coordinator_state{tables = [Table | Tables]},
    {noreply, NewState};

handle_cast({remove_table, Table_pid}, State) ->
    Tables = State#coordinator_state.tables,
    case lists:keyfind(Table_pid, 1, Tables) of
	false ->
	    {reply, no_such_table_exists, State};
	_Table -> 
	    NewTables = lists:keydelete(Table_pid, 1, Table_pid),
	    NewState = State#coordinator_state{tables = NewTables},
	    gen_server:cast(Table_pid, exit),
	    {reply, removal_succeeded, NewState}
    end.


handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

