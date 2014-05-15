-module(game_state).
-export([state/3]).

state(Tick, State_sender, Db_name, State) ->
    Time = erlang:now(),
    Actions = action_db:get_actions(Db_name),
    New_state = game_logic:do_actions(State, Actions),
    State_sender ! {new_state, New_state},
    Sleep_time = ((1000000 div Tick) - 
		     timer:now_diff(erlang:now(), Time))div 1000,  
    if Sleep_time > 0 ->
	    timer:sleep(Sleep_time);
       true ->
	    ok %% no sleep
    end,
    game_state(Tick, State_sender, Db_name, New_state).


