%%====================================================================
%%
%% @author Juanse Perez Herrero <juanseph@gmail.com> [http://bytefilia.com]
%% @copyright CC Attribution - 2013
%%
%% A sample otp gen_server template
%%
%%====================================================================
-module(geese_dispatcher).
-behaviour(gen_server).
-compile(export_all).
% interface calls
%-export([start_link/2, stop/0, accept_function/3, accept_spawner/1]).
    
% gen_server callbacks
-export([init/1,
         handle_call/3, 
         handle_cast/2,
         handle_info/2, 
         terminate/2, 
         code_change/3]).

%%-record(state, {port, listen_socket}).


%%====================================================================
%% API
%%====================================================================

%spawn_client(Socket, Port, Destination_ip, Destination_port, Player_name) ->
%    spawn_link(geese_player, init_player, [Socket, Port, DestinationIp, DestinationPort, self()]).


%%====================================================================
%% Server interface
%%====================================================================
%% Booting server (and linking to it)
start_link(Port, Server_pid) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, Server_pid], []).

%% Stopping server asynchronously
stop() ->
    gen_server:cast(?MODULE, shutdown).



%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Port, Server_pid]) ->
    {ok, Listen_socket} = gen_tcp:listen(Port, [{active, true}, {reuseaddr, true}, {packet, 0}]),
    accept_spawner(Listen_socket, Server_pid),
    {ok, {Listen_socket, Server_pid}}.

accept_spawner(Listen_socket, Server_pid) ->
   % [spawn(fun() -> accept_function(Listen_socket, self(), Index) end) || Index <- lists:seq(1, 20)].
    proc_lib:spawn(?MODULE, accept_function, [Listen_socket, self(), 1, Server_pid]),
    Listen_socket.
		  
%fun() -> accept_function(Listen_socket, self()) end).
%    accept_function(Listen_socket, self()),
%    proc_lib:spawn(?MODULE, accept_function, [{Listen_socket, self()}]).

accept_function(Listen_socket, Dispatcher_pid, Index, Server_pid) ->
    case gen_tcp:accept(Listen_socket) of
	{ok, Accept_socket} -> 
	    gen_server:cast(Dispatcher_pid, {accept, Listen_socket, Dispatcher_pid}),
	    {ok, Player_pid} = geese_player:start_link(Accept_socket, Server_pid, self()),
	    gen_tcp:controlling_process(Accept_socket, Player_pid),
	    gen_server:cast(Player_pid, {start_player, Accept_socket, Dispatcher_pid});
	{error, Reason} ->
	    io:format("~n~nreason why accept failed: ~p~n~n", [Reason]) 
    end.


handle_cast({accept, Listen_socket, _Dispatcher_pid}, {_Listen_socket, Server_pid}) -> io:format("cast, accept, dispatcher, pid: ~p", [self()]),
  %  proc_lib:spawn(fun() -> accept_function(Listen_socket, self(), 100, Server_pid) end),
    {noreply, {accept_spawner(Listen_socket, self()), Server_pid}};

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

%% Server termination
terminate(_Reason, _Server) -> 
    io:format("Generic termination handler: '~p' '~p'~n",[_Reason, _Server]).


%% Code change
code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.
