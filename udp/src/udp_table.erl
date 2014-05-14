%%%-------------------------------------------------------------------
%%% File    : eb_server.erl
%%% Author  : Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%% Description : The ErlyBank account server.
%%%
%%% Created :  5 Sep 2008 by Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%%-------------------------------------------------------------------
-module(udp_table).

-behaviour(gen_server).

%% API
-export([start_link/0, checkout/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% players = list of player_pids
%% game = udp_game-instance
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(table_state, {
	  number_of_players,
	  max_players,
	  game,
	  players = []}).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

checkout(Who, Book) -> gen_server:call(?MODULE, {checkout, Who, Book}).	

init([]) ->
						%    proc_lib:start(fun() -> udp_game:test() end),
    {ok, #table_state{number_of_players = 0, max_players = 20}}.
	
handle_call({player_token_to_pid, Token}, _From, State) ->
    Players = State#table_state.players,
    {Pid, Token} = lists:keyfind(Token, 2, Players),
    {reply, Pid, State};


handle_call({add_player, Player_id}, From, State) ->
    Players = State#table_state.players,
    NewState = State#table_state{players = [{From, Player_id} | Players]},
    {reply, ok, NewState}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
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

