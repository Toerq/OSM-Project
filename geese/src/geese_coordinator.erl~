%%%-------------------------------------------------------------------
%%% File    : eb_server.erl
%%% Author  : Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%% Description : The ErlyBank account server.
%%%
%%% Created :  5 Sep 2008 by Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%%-------------------------------------------------------------------
-module(udp_coordinator).

-behaviour(gen_server).

%% API
-compile(export_all).
-export([start_link/0, checkout/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, test/0]).

-record(coordinator_state,
	{players = [],
	 tables = [],
	 player_at_table = [],
	 test}
).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
checkout(Who, Book) -> gen_server:call(?MODULE, {checkout, Who, Book}).	
join_lobby(Pid) -> gen_server:call(?MODULE, {join_lobby_2, Pid}).
browse_tables() -> gen_server:call(?MODULE, browse_tables).
add_player_to_table(Player_id, Table_ref) -> gen_server:call(?MODULE, {add_player_to_table, Player_id, Table_ref}).
add_table() -> gen_server:cast(?MODULE, add_table).
browse_players() -> gen_server:call(?MODULE, browse_players).
    
    
init([]) ->
  {ok, #coordinator_state{test = ja}}.


handle_call(browse_players, _From, State) ->
   {reply, State#coordinator_state.players, State};

handle_call(browse_tables, _From, State) ->
        {reply, State#coordinator_state.tables, State};


%%{table_settings, {max_players, game_type, tick_rate(?), etc}}

handle_call(lewut, _From, State) ->
    io:format("lewut"),
    {reply, bbq, State};

handle_call({add_player_to_table, Player_id, Table_ref}, _From, State) ->
    Tables = State#coordinator_state.tables,
    
    case gen_server:call(Table_ref, {add_player, Player_id}) of
	{add_succeded} ->
	    {reply, {add_succes}, State};
	{add_failed, Reason} ->
	    {reply, {add_failed, Reason}, State}
    end;

handle_call({join_lobby_2, Pid}, _From, State) ->
    Player_id = make_ref(),
    Players = State#coordinator_state.players,
    NewState = State#coordinator_state{players = [{Pid, Player_id} | Players]},
    {reply, Player_id, NewState};


handle_call(join_lobby, From, State) ->
    io:format("~nI joinlobby, From: ~p, State: ~p", [From, State]),
    {Pid, _From} = From,
    Player_id = make_ref(),
    Players = State#coordinator_state.players,
    NewState = State#coordinator_state{players = [{Pid, Player_id} | Players]},
    {reply, Player_id, NewState}.

test() ->
    io:format("~nprint från coordinator~p~n", [self()]).

handle_cast(add_table, State) ->
    %% TODO: case sats för ifall table-skapandet misslyckas,
    %% implementera udp_table:init (working title)
%case udp_table:init(table_ref), of ....
%{Table_ref, Current_nmbr_of_players, Max_players, 
    %%Game_type, Players, *referens till bordet}
    {ok, Table_pid} = udp_table:start_link(),
    io:format("~n~p~n", [Table_pid]),
    Tables = State#coordinator_state.tables,
    Table = {Table_pid, Name, mmo_tetris, 0, 20, mmo_tetris, []},
    Tables =  State#coordinator_state.tables,
    NewState = State#coordinator_state{tables = [Table | Tables]},
    {noreply, NewState}.
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
