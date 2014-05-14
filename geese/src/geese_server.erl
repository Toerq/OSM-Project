%%  c(geese_player), c(geese_coordinator), c(geese_player), c(geese_table), c(geese_dispatcher), c(geese_server), geese_server:start(3010).

%%====================================================================
%%
%% @author Juanse Perez Herrero <juanseph@gmail.com> [http://bytefilia.com]
%% @copyright CC Attribution - 2013
%%
%% A sample otp gen_server template
%%
%%====================================================================
-module(geese_server).
-behaviour(gen_server).

% interface calls
-export([start/0, start/1, stop/0]).
    
% gen_server callbacks
-export([init/1,
         handle_call/3, 
         handle_cast/2,
         handle_info/2, 
         terminate/2, 
         code_change/3,
	 spawn_client/3]).

-record(state, {port, listen_socket, dispatcher, coordinator
	       }).


-define(DEFAULT_PORT, 41526).
-define(LOCAL_HOST, {127,0,0,1}).

%%====================================================================
%% API
%%====================================================================

spawn_client(Socket, Destination_ip, Destination_port) ->
    spawn_link(geese_player, init_player, [Socket, Destination_ip, Destination_port, self()]),
       io:format("pid from process: ~p", [self()]).


%%====================================================================
%% Server interface
%%====================================================================
%% Booting server (and linking to it)
start(Port) -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

start() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [?DEFAULT_PORT], []).

%% Stopping server asynchronously
stop() ->
    gen_server:cast(?MODULE, shutdown).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Port]) ->
    Coordinator = geese_coordinator:start_link(),
    Dispatcher = geese_dispatcher:start_link(Port, self()),
    io:format("~nprint från server~p~n, Coordinator pid: ~p~n", [self(), Coordinator]),
    {ok, #state{port = Port, coordinator = Coordinator, dispatcher = Dispatcher} }.
    

handle_call(get_state, _From, State) ->
    {reply, State#state.coordinator, State};

handle_call(Message, From, State) -> 
    io:format("Generic call handler: '~p' from '~p' while in '~p'~n",
    [Message, From, State]),
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
