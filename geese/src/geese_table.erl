-module(geese_table).

-behaviour(gen_server).

-export([get_state/0, get_players/0, join_table/3, remove_player/1]).

-export([start_link/1, start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(table_state, {
	  number_of_players,
	  max_players,
	  state_sender,
	  db_name,
	  players = []}).

-define(SERVER, ?MODULE).
-define(TICKRATE, 32).


start_link(Max_players, debug) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Max_players], []).

%% @doc Initiates a game table with the maximum amount of allowed players Max_players. Also, initiate a new game_state process.
-spec start_link(Max_players::integer()) -> {ok, pid()}.
start_link(Max_players) ->
    gen_server:start(?MODULE, [Max_players], []).

init([Max_players]) ->
    State_sender = spawn(fun() -> game_state:state_sender(game_logic:make_new_state()) end),
    Db_name = action,
    Tick = ?TICKRATE,
    game_state:start(Db_name, State_sender, Tick),
    {ok, #table_state{number_of_players = 0, max_players = Max_players, state_sender = State_sender, db_name = Db_name, players = []}}.

%% Används endast för testning
get_state() -> gen_server:call(?MODULE, get_state).
get_players() -> gen_server:call(?MODULE, get_players).
join_table(Pid, Player_name, Socket) -> gen_server:call(?MODULE, {join_table, Pid, Player_name, Socket}).
remove_player(Pid) -> gen_server:call(?MODULE, {remove_player, Pid}).

%% @doc Synchronous call-handlers.
%% === Case {get_state} ===
%% <div class="example">
%% Returns the current state of the table
%% </div>
%% === Case {get_players} ===
%% <div class="example">
%% Returns the current players in the table on the form [{Pid0, Player_name0}, ..., {PidN, Player_nameN}.
%% </div>
%% === Case {available_slots} ===
%% <div class="example">
%% Returns a tuple which consists of the number of players in the table, the number of max allowed players and the number of available slots.
%% </div>
%% === Case {join_table, Pid, Player_name, } ===
%% <div class="example">
%% Returns the current players in the table on the form [{Pid0, Player_name0}, ..., {PidN, Player_nameN}.
%% </div>
handle_call(get_state, _From, State) ->
    {reply, {State#table_state.players, 
	     State#table_state.number_of_players, 
	     State#table_state.max_players},
     State};

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

%% @doc Adds a new player to the table if Max players > Number of players
handle_call({join_table, Pid, Player_name}, _From, State) ->
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
	    NewPlayerList = lists:keydelete(Pid, 1, Players),
	    NewState = State#table_state{players = NewPlayerList, number_of_players = New_number_of_players},
	    {reply, removal_succeeded, NewState}
    end.
handle_cast(exit, State) ->
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

