-module(stress_test).

-compile(export_all).


start(Number_of_processes, Number_of_iterations) ->
     [spawn(fun() -> connect_function(Number_of_iterations) end) || _ <- lists:seq(1, Number_of_processes)].


start() ->
     [spawn(fun() -> connect_function(20) end) || _ <- lists:seq(1, 40)].

connect_function(0) ->
    done;

connect_function(N) ->
    gen_tcp:connect({127,0,0,1}, 3010, []),
    connect_function(N - 1).
