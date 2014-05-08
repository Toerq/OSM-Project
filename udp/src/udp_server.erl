%%====================================================================
%%
%% @author Juanse Perez Herrero <juanseph@gmail.com> [http://bytefilia.com]
%% @copyright CC Attribution - 2013
%%
%% A sample otp gen_server template
%%
%%====================================================================
-module(udp_server).
-behaviour(gen_server).

% interface calls
-export([start/1, stop/0]).
    
% gen_server callbacks
-export([init/1,
         handle_call/3, 
         handle_cast/2,
         handle_info/2, 
         terminate/2, 
         code_change/3,
	 spawn_client/3]).

-record(state, {port, listen_socket}).


%%====================================================================
%% API
%%====================================================================

spawn_client(Socket, Destination_ip, Destination_port) ->
    spawn_link(udp_player, init_player, [Socket, Destination_ip, Destination_port, self()]).


%%====================================================================
%% Server interface
%%====================================================================
%% Booting server (and linking to it)
start(Port) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

%% Stopping server asynchronously
stop() ->
    gen_server:cast(?MODULE, shutdown).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Port]) ->
{ok, Socket} = gen_udp:open(Port, "localhost"),
{ok, #state{port = Port, listen_socket = Socket}}.


%% Synchronous, possible return values  
% {reply,Reply,NewState} 
% {reply,Reply,NewState,Timeout}
% {reply,Reply,NewState,hibernate}
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,Reply,NewState} 
% {stop,Reason,NewState}
handle_call(Message, From, State) -> 
    io:format("Generic call handler: '~p' from '~p' while in '~p'~n",[Message, From, State]),
    {reply, ok, State}.

%% Asynchronous, possible return values
% {noreply,NewState} 
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
%% normal termination clause
handle_cast(shutdown, State) ->
    io:format("Generic cast handler: *shutdown* while in '~p'~n",[State]),
    {stop, normal, State};
%% generic async handler
handle_cast(Message, State) ->
    io:format("Generic cast handler: '~p' while in '~p'~n",[Message, State]),
    {noreply, State}.

%% Informative calls
% {noreply,NewState} 
% {noreply,NewState,Timeout} 
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
handle_info(_Message, _Server) -> 
    io:format("Generic info handler: '~p' '~p'~n",[_Message, _Server]),
    {noreply, _Server}.

%% Server termination
terminate(_Reason, _Server) -> 
    io:format("Generic termination handler: '~p' '~p'~n",[_Reason, _Server]).


%% Code change
code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.
