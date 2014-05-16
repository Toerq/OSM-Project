-module(game_state).
-export([state/4, start/3, register_action/1]).

start(Db_name, State_sender, Tick_rate) ->
    action_db:init(Db_name),
    State = game_logic:make_new_state(),
    Pid = spawn(fun() ->
			state(Tick_rate, State_sender, Db_name, State)
		end),
    {ok, Pid}.

register_action(Action) ->
    action_db:do_call(Action).
    

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
    state(Tick, State_sender, Db_name, New_state).


