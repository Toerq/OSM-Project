%%%-------------------------------------------------------------------
%%% File    : eb_server.erl
%%% Author  : Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%% Description : The ErlyBank account server.
%%%
%%% Created :  5 Sep 2008 by Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%%-------------------------------------------------------------------
-module(geese_table).

-behaviour(gen_server).

%% API

-compile(export_all).
-export([start_link/1, start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% players = list of player_pids
%% game = geese_game-instance
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(table_state, {
	  number_of_players,
	  max_players,
	  state_sender,
	  db_name,
	  players = []}).

-define(SERVER, ?MODULE).
-define(TICKRATE, 32).

%% @doc Initiates a game table with the maximum amount of allowed players Max_players.

%%TODO: Fixa -ifdef(debug)
start_link(Max_players, debug) ->
       gen_server:start_link({local, ?SERVER}, ?MODULE, [Max_players], []).
    
start_link(Max_players) ->
    gen_server:start(?MODULE, [Max_players], []).

%%TODO: fixa ifdef(debug)
get_state() -> gen_server:call(?MODULE, get_state).
get_players() -> gen_server:call(?MODULE, get_players).
join_table(Pid, Player_name, Socket) -> gen_server:call(?MODULE, {join_table, Pid, Player_name, Socket}).
remove_player(Pid) -> gen_server:call(?MODULE, {remove_player, Pid}).



init([Max_players]) ->
    State_sender = spawn(fun() -> game_state:state_sender(game_logic:make_new_state()) end),
    %%Db_name = list_to_atom(erlang:ref_to_list(make_ref())),
    Db_name = action,
    Tick = ?TICKRATE,
    game_state:start(Db_name, State_sender, Tick),
    {ok, #table_state{number_of_players = 0, max_players = Max_players, state_sender = State_sender, db_name = Db_name, players = []}}.


handle_call(get_state, _From, State) ->
    {reply, {State#table_state.players, 
	     State#table_state.number_of_players, 
	     State#table_state.max_players},
	     State};

%% Används inte
%%handle_call({check_player, Player_pid}, _From, State) ->
%%    Players = State#table_state.players,
%%    case lists:keyfind(Player_pid, 1, Players) of
%%	false ->
%%	    {reply, player_not_in_table, State};
%%	_E ->
%%	    {reply, player_already_in_table, State}
%%    end;

%% @doc Returns all players in table with a list of tuples on the form: [{Pid0, Player_name0}, ..., {PidN, Player_nameN}
handle_call(get_players, _From, State) ->
    {reply, State#table_state.players, State};

%% @doc Returns a tuple which consists of the number of players in the table, the number of max allowed players and the number of available slots.
handle_call(available_slots, _From, State) ->
    Number_of_players = State#table_state.number_of_players,    
    Max_players = State#table_state.max_players,
    Number_of_slots = Max_players - Number_of_players,
    {reply, {Number_of_players, Max_players, Number_of_slots}, State};

handle_call(db_name, _From, State) ->
    {reply, State#table_state.db_name, State};

%%Bör byta namn till add_player
%% @doc Adds a new player to the table if Max players > Number of players
handle_call({join_table, Pid, Player_name, _Socket}, _From, State) ->
    Number_of_players = State#table_state.number_of_players,
    Max_players = State#table_state.max_players,
    if (Max_players > Number_of_players) -> 
	    Players = State#table_state.players,
	    case lists:keyfind(Pid, 1, Players) of
		false ->
		    New_number_of_players = Number_of_players + 1,

		    NewState = State#table_state{players = [{Pid, Player_name} | Players], number_of_players = New_number_of_players},
		    State_sender = State#table_state.state_sender,
		    Db_name = State#table_state.db_name,
		    Return_tuple = {self(), State_sender, Db_name},

		    {reply, Return_tuple, NewState};
	    	_E ->
	    	    {reply, join_failed, State}
	    end;
       true ->
%%	    io:format("~ntoo many players, max_players: ~p, nmbr_of_players: ~p~n", [Max_players, Number_of_players]),
	    {reply, join_failed, State}
    end;

handle_call({remove_player, Pid}, _From, State) ->
    Players = State#table_state.players,
    case lists:keyfind(Pid, 1, Players) of
	false -> 
	    {reply, no_such_player_exists, State};
	_ ->
	    Number_of_players = State#table_state.number_of_players,
	    New_number_of_players = Number_of_players - 1,
            %register_action(Db_name, %% ADD_PLAYER %%),
	    NewPlayerList = lists:keydelete(Pid, 1, Players),
	    NewState = State#table_state{players = NewPlayerList, number_of_players = New_number_of_players},
	    {reply, removal_succeeded, NewState}
    end.
handle_cast(exit, State) ->
    %% call game module to shut down its' processes.
    exit(requested_shutdown),
    {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

