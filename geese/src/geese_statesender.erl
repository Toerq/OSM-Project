-module(geese_statesender).


-compile(export_all).

-define(SERVER, ?MODULE).
-define(TICK, 32).

start(Socket, Db_name) ->
  proc_lib:start(fun() -> send_loop(Socket, Db_name) end).

send_loop(Socket, Db_name) -> 
    Time = erlang:now(),
    State = action_db:get_actions(Db_name),
    gen_tcp:send(Socket, State),
    SleepTime = ((1000000 div ?TICK) - timer:now_diff(erlang:now(), Time))div 1000,  
    if SleepTime > 0 ->
	    timer:sleep(SleepTime);
       true ->
	    ok %% no sleep
    end,
    send_loop(Socket, Db_name).
