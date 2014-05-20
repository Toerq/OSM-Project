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
-export([start_link/0, checkout/2]).

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
	  state_sender,
	  max_players,
	  db_name,
	  players = []}).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

checkout(Who, Book) -> gen_server:call(?MODULE, {checkout, Who, Book}).	

init([]) ->
    State_sender = spawn(fun() -> game_state:state_sender(game_logic:make_new_state()) end),
    Db_name = server01,
    Tick = 30,
    {ok, _} = game_state:start(Db_name, State_sender, Tick),
    {ok, #table_state{number_of_players = 0, max_players = 20, state_sender = State_sender, db_name = Db_name, players = []}}.


handle_call(get_state, _From, State) ->
    {reply, {State#table_state.players, 
	     State#table_state.number_of_players, 
	     State#table_state.max_players},
	     State};

handle_call({check_player, Player_pid}, _From, State) ->
    Players = State#table_state.players,
    case lists:keyfind(Player_pid, 1, Players) of
	false ->
	    {reply, player_not_in_table, State};
	_E ->
	    {reply, player_already_in_table, State}
    end;

handle_call(get_players, _From, State) ->
    {reply, State#table_state.players, State};

handle_call(available_slots, _From, State) ->
    Number_of_players = State#table_state.number_of_players,    
    Max_players = State#table_state.max_players,
    Number_of_slots = Max_players - Number_of_players,
    {reply, {Number_of_players, Max_players, Number_of_slots}, State};

handle_call(db_name, _From, State) ->
    {reply, State#table_state.db_name, State};

handle_call({join_table, Pid, Player_name, _Socket}, _From, State) ->
    Number_of_players = State#table_state.number_of_players,
    Max_players = State#table_state.max_players,
    if (Max_players > Number_of_players) 
       -> 
	    Players = State#table_state.players,
	    case lists:keyfind(Pid, 1, Players) of
		false ->
	    New_number_of_players = Number_of_players + 1,
	    
	    NewState = State#table_state{players = [{Pid, Player_name} | Players], number_of_players = New_number_of_players},
	    {reply, ok, NewState};
	    	_E ->
	    	    {reply, join_failed, State}
	    end;
       	    %Db_name = State#table_state.db_name,
            %register_action(Db_name, %% ADD_PLAYER %%),
	    
	    
	    %Players = State#table_state.players,
	    
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

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

