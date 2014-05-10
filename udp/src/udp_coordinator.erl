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
-export([start_link/0, checkout/2, join_lobby/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(coordinator_state,
	{players = [],
	 tables = [],
	 player_at_table = [],
	 test}
).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

checkout(Who, Book) -> gen_server:call(?MODULE, {checkout, Who, Book}).	
join_lobby() -> gen_server:call(?MODULE, join_lobby).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  {ok, #coordinator_state{test = ja}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({standard, Message}, From, State) -> 
    io:format("Generic call handler: '~p' from '~p' while in '~p'~n",
    [Message, From, State]),
    {reply, ok, State};

handle_call(browse_tables, _From, State) ->
    Tables = State#coordinator_state.tables,
    
    {reply, {ok, Tables}, State};


%%{table_settings, {max_players, game_type, tick_rate(?), etc}}

handle_call({add_player_to_table, Player_id, Table_ref}, _From, _State) ->
%%    {Pid, _From} = From,
    case gen_server:call(Table_ref, Player_id) of
	{add_succeded} ->
	    {add_succes};
	{add_failed, Reason} ->
	    {add_failed, Reason} 
    end;

handle_call(join_lobby, From, State) ->
    {Pid, _From} = From,
    Player_id = make_ref(),
    Players = State#coordinator_state.players,
    NewState = State#coordinator_state{players = [{Pid, Player_id} | Players]},
    io:format("_________________vi kom hit___________________"),
    {reply, {ok, Player_id}, NewState}.


    
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
%ha som call istället, där caller måste hantera ifall table-create misslyckades.
handle_cast(add_table, State) ->
    %% TODO: case sats för ifall table-skapandet misslyckas,
    %% implementera udp_table:init (working title)
%case udp_table:init(table_ref), of ....
%{Table_ref, Current_nmbr_of_players, Max_players, 
    %%Game_type, Players, *referens till bordet}
    Table = {make_ref(), 0, 20, mmo_tetris, []},
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
