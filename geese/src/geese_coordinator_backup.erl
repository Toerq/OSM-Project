%% @author Christian Törnqvist
-module(geese_coordinator_backup).

-behaviour(gen_server).

-export([start_link/0, stop/1]).
-export([back_up/1, get_state/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
            code_change/3]).

-define(SERVER, ?MODULE).

%% @doc This module is responsible of keeping the state of the coordinator state as a backup if the coordinator module would crash.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop(_Reason) -> 
    tbi.

back_up(State) ->
    gen_server:cast(?MODULE, State).

%% @doc Retrieve the state stored in this server. If there is a state stored
%% here, it is returned to the caller. If the backup server does not have a
%% state stored, it will return the no_state_stored atom.
get_state() ->
    gen_server:call(?MODULE, get_state).

init([]) ->
    {ok, no_state_saved}.

handle_call(get_state, _From, State) ->
    {reply, State, State}.

handle_cast(NewState, _State) ->
    {noreply, NewState}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
