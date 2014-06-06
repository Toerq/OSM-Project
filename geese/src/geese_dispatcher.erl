%% @author Christian Törnqvist
-module(geese_dispatcher).
-behaviour(gen_server).

-export([start_link/1, stop/0]).

-export([init/1,
         handle_call/3, 
         handle_cast/2,
         handle_info/2, 
         terminate/2, 
         code_change/3]).

%% @doc Initiates a new Dispatcher which accepts connections and starts a new player process for each client that connects.
-spec start_link(Port::integer()) -> {ok, pid()}.
start_link(Port) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

stop() ->
    gen_server:cast(?MODULE, shutdown).

init([Port]) ->
    process_flag(trap_exit, true),
    {ok, Listen_socket} = gen_tcp:listen(Port, [binary,
						{packet, 0},
						{reuseaddr, true},
						{active, true}]),
    accept_spawner(Listen_socket),
    {ok, {Listen_socket}}.

%% @doc Spawns 20 new processes that listens for new connections.
accept_spawner(Listen_socket) ->
    [spawn(fun() -> accept_function(Listen_socket, self()) end) || _ <- lists:seq(1, 20)].

%% @doc Waits for a client to connect through the TCP-protocol. Starts a new player process for each client that gets accepted.
accept_function(Listen_socket, Dispatcher_pid) ->
    {ok, Accept_socket} = gen_tcp:accept(Listen_socket),
    case geese_player:start_link(Accept_socket) of
	{ok, Player_pid} ->
	    gen_tcp:controlling_process(Accept_socket, Player_pid),
	    gen_server:cast(Player_pid, start_player),
	    accept_function(Listen_socket, Dispatcher_pid);
	E ->
	    io:format("~nSomething went wrong: ~p~n", [E])
    end.
 
handle_cast({accept, Listen_socket, _Dispatcher_pid}, _State) -> 
    {noreply, {accept_spawner(Listen_socket)}}.

handle_call(_Message, _From, State) -> 
    {reply, ok, State}.

handle_info(_Message, _Server) -> 
    io:format("Generic info handler: '~p' '~p'~n",[_Message, _Server]),
    {noreply, _Server}.

terminate(_Reason, _Server) -> 
    io:format("Generic termination handler: '~p' '~p'~n",[_Reason, _Server]).

code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.
