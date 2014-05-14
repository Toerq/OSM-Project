%%%-------------------------------------------------------------------
%%% File    : eb_server.erl
%%% Author  : Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%% Description : The ErlyBank account server.
%%%
%%% Created :  5 Sep 2008 by Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%%-------------------------------------------------------------------
-module(udp_player).

-behaviour(gen_server).

-compile(export_all).
%% API
-export([start_link/3, checkout/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {player_id, accept_socket, server_pid, coordinator}).

-define(SERVER, ?MODULE).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Accept_socket, Server_Pid, Dispatcher_pid) ->
    gen_server:start(?MODULE, [Accept_socket, Server_Pid, Dispatcher_pid], []).
%    gen_server:start({local, ?SERVER}, ?MODULE, [Accept_socket, Server_Pid, Dispatcher_pid], []).

checkout(Who, Book) -> gen_server:call(?MODULE, {checkout, Who, Book}).	

init([Accept_socket, Server_pid, _Dispatcher_pid]) ->
%  Coordinator = gen_server:call(Server_pid, get_state),
   % gen_server:call(Dispatcher_pid, lewut),
   % Pid = proc_lib:spawn_link(fun() -> greet_state(Accept_socket, Dispatcher_pid) end),
    %udp_coordinator:join_lobby(Pid),
    %gen_tcp:controlling_process(Accept_socket, Pid),
    {ok, #state{player_id = layer_id, accept_socket = Accept_socket, server_pid = Server_pid, coordinator = oordinator}}.

%talk_state() -> gen_server:cast(?MODULE, talk_state).

talk_state(State) -> %handle_cast({talk_state}, State) ->
    Accept_socket = State#state.accept_socket,
    gen_tcp:send(Accept_socket, "~n~nalternatives: add_table, ping, browse_tables~n"),
    receive
	{tcp, Accept_socket, Packet} ->
	    case Packet of
		"add_table" -> 
		    udp_coordinator:add_table(),
		    gen_tcp:send(Accept_socket, "~nUpdated tables:"),
		    Tables = udp_coordinator:browse_tables(),
		    String1 = lists:flatten(io_lib:format("~p~n", [Tables])),
		    gen_tcp:send(Accept_socket, String1);
		"ping" ->
		    gen_tcp:send(Accept_socket, "pong");
		"browse_tables" ->
		    Tables = udp_coordinator:browse_tables(),
		    String1 = lists:flatten(io_lib:format("~p~n", [Tables])),
		    gen_tcp:send(Accept_socket, String1);
		{join_server, Table_ref} ->
		    %% add player to table,
		    %%enter game_state(Table_ref)
		    tbi
	    end
    end,
    talk_state(State).
    
game_state(Table_ref, Accept_socket) ->
tbi.
%    recieve
%	{tcp, Accept_socket, Actions} ->
					     %%udp_game(Actions),
					     %%{game_state, State},
					     %% gen_tcp:send(Accept_socket, State).
					     %%game_state(Table_ref, Accept_socket)

handle_cast({start_player, Accept_socket, _Dispatcher_pid}, State) ->
    Player_id = udp_coordinator:join_lobby(self()),
    NewState = State#state{player_id = Player_id},
    gen_tcp:send(Accept_socket, "mhhh"),
    Tables = udp_coordinator:browse_tables(),
    String1 = lists:flatten(io_lib:format("Tillgängliga servrar: ~p~n", [Tables])),
    gen_tcp:send(Accept_socket, String1), 
    Players = udp_coordinator:browse_players(),
    
    String2 = lists:flatten(io_lib:format("Tillgängliga spelare: ~p~n", [Players])),
    gen_tcp:send(Accept_socket, String2),
    talk_state(NewState),
    {noreply, NewState};

handle_cast({greet_state, Accept_socket, _Dispatcher_pid}, State) ->
    %Coordinator = State#state.coordinator,
    gen_tcp:send(Accept_socket, "mhhh"),
    %gen_server:call(Dispatcher_pid, aa),
%    io:format("asdasdasdad"),
						%  %  NewState = State#state{player_id = Player_id},
    
%    gen_tcp:send(Accept_socket, io_lib:format("Tillgängliga servrar före: " ++ Tables ++ "~n")), 
    udp_coordinator:add_table(),
    Tables2 = udp_coordinator:browse_tables(),%gen_server:call(Coordinator, browse_tables),
 %   gen_tcp:send(Accept_socket, io_lib:format("Tillgängliga servrar efter: " ++ Tables2 ++ "~n")), 
    io:format("~nEfter: ~p", [Tables2]),
    receive
	{tcp, Accept_socket, Data} ->
	    io:format("Got packet: ~p", [Data])
    end,
    %NewTables = gen_server:call(Coordinator, browse_tables),
    %gen_server:call(Coordinator, add_table),
    %gen_tcp:send(Accept_socket, io_lib:format("~nTillgängliga servrar efter: " ++ NewTables)), 
%    {reply, Reply} = gen_server:call(self(), {recieve_choice_state, Accept_socket}, infinity),
    {noreply, State}.

greet_state(Accept_socket, Dispatcher_pid) ->
    gen_server:cast(?MODULE, {greet_state, Accept_socket, Dispatcher_pid}).
    


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
