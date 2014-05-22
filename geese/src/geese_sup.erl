%% c(geese_player), c(geese_coordinator), c(geese_player), c(geese_table), c(geese_dispatcher), c(game_state), c(game_logic), c(action_db), c(geese_server), c(geese_coordinator_backup), c(geese_sup), geese_sup:start_link(3010).

-module(geese_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Port) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Port).

init(Port) ->
    {ok, {{one_for_one, 3, 60},
	 [
	   {geese_coordinator_backup,
	    {geese_coordinator_backup, start_link, []},
	    permanent, 1000, worker, [geese_coordinator_backup]},
	   {geese_coordinator,
	    {geese_coordinator, start_link, []},
	    permanent, 1000, worker, [geese_coordinator]},
	   {geese_dispatcher,
	    {geese_dispatcher, start_link, [Port, foo]},
	    permanent, 1000, worker, [geese_dispatcher]}
%%	  {geese_player_sup,
%%	   {geese_player_sup, start_link, []},
%%	   permanent, 1000, supervisor, [geese_player_sup]}
	 %% {geese_table_sup,
	 %%   {geese_table_sup, start_link, []},
	 %%  permanent, 1000, supervisor, [geese_table_sup]}

	  ]}}.
