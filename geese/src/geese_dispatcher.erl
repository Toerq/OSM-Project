-module(geese_dispatcher).
-behaviour(gen_server).

%%Ta bort compile efter development-state
-compile(export_all).
-export([init/1,
         handle_call/3, 
         handle_cast/2,
         handle_info/2, 
         terminate/2, 
         code_change/3]).

start_link(Port) -> 
    io:format("start_link dispatcher"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

stop() ->
    gen_server:cast(?MODULE, shutdown).

init([Port]) ->
    %% To know when the parent shuts down
    process_flag(trap_exit, true), %% <- behövs detta?
    {ok, Listen_socket} = gen_tcp:listen(Port, [binary,
						{packet, 0},
						{reuseaddr, true},
						{active, true}]),
    accept_spawner(Listen_socket),
    {ok, {Listen_socket}}.

accept_spawner(Listen_socket) ->
    [spawn(fun() -> accept_function(Listen_socket, self()) end) || _ <- lists:seq(1, 20)].
    		  
accept_function(Listen_socket, Dispatcher_pid) ->
    {ok, Accept_socket} = gen_tcp:accept(Listen_socket),
    case geese_player:start_link(Accept_socket) of
	{ok, Player_pid} ->
%%	    io:format("~n Pid from spawned player: ~p~n", [Player_pid]),
	    gen_tcp:controlling_process(Accept_socket, Player_pid),
	    gen_server:cast(Player_pid, start_player),
	    accept_function(Listen_socket, Dispatcher_pid);
	E ->
	    io:format("~nSomething went wrong: ~p~n", [E])
    end.
 
handle_cast({accept, Listen_socket, _Dispatcher_pid}, _State) -> 
    io:format("cast, accept, dispatcher, pid: ~p", [self()]),
    {noreply, {accept_spawner(Listen_socket)}};

handle_cast({standard, Message}, State) -> 
    io:format("Generic call handler: '~p' while in '~p'~n", 
	      [Message, State]),
    {noreply, State}.
  
handle_call(Message, From, State) -> 
    io:format("Generic call handler: '~p' from '~p' while in '~p'~n",[Message, From, State]),
    {reply, ok, State}.

handle_info(_Message, _Server) -> 
    io:format("Generic info handler: '~p' '~p'~n",[_Message, _Server]),
    {noreply, _Server}.

terminate(_Reason, _Server) -> 
    io:format("Generic termination handler: '~p' '~p'~n",[_Reason, _Server]).

code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.
