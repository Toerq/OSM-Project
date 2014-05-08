-module(tcp_udp_server).

-export([start/0, stop/0, game_state/4, state_sender/0, game_tick_once/1, do_call/1, available/0, add/2, remove/1, state_generator/2, action_generator/1]).

-include("actions.hrl").

start() ->
    mnesia:start(),
    Pid = spawn(action_handler_loop),
    {pid, Pid}.

stop() ->
    mnesia:stop().

action_handler_loop() ->
    receive
	

%%input_handler(Socket) ->
%%    receive
%% 	{tcp, Socket, Bin} ->
%% 	    Term = binary_to_term(Bin),
%%	    Reply = do_call(Term),
%% 	    send_term(Socket, Reply),
%%	    input_handler(Socket);
%% 	{tcp_closed, Socket} ->
%%	    true
%%     end.

game_tick_once(State) ->
    Time = erlang:now(),
    {_,Actions} = do_call({a_available}), %% mnesia:foldl(fun(X,XS) -> [X|XS] end, [], actions),
    NewState = doActions(State, Actions),
    {ok, timer:now_diff(erlang:now(), Time), NewState}.  

game_state(_Tick, _StateSender, State, 0) ->
    State;
game_state(Tick, StateSender, State, N) ->
    Time = erlang:now(),
    {_,Actions} = do_call({a_available}), %% mnesia:foldl(fun(X,XS) -> [X|XS] end, [], actions),
    NewState = doActions(State, Actions),
    %% StateSender ! {new_state, NewState},
    SleepTime = ((1000000 div Tick) - timer:now_diff(erlang:now(), Time))div 1000,  
    StateSender ! {new_state, SleepTime},  
    if SleepTime > 0 ->
	    timer:sleep(SleepTime)
    end,
    game_state(Tick, StateSender, NewState, N-1).

doActions(State, []) ->
    State;
doActions({ServerSettings, PlayerList}, [A | T]) ->
    doActions({ServerSettings, doActions(ServerSettings, PlayerList, [], A)}, T).

doActions(_ServerSettings, [], PList, _Action)->
    PList;
doActions(ServerSettings, [{P, X, Y} | T], PList, {_, Player, Action}) when P =:= Player ->
    doActions(ServerSettings, T, [doAction(ServerSettings, {P, X, Y}, Action) | PList], {actions, Player, Action});
doActions(ServerSettings, [{P, X, Y} | T], PList, {Player, Action}) when Player =:= global ->
    doActions(ServerSettings, T, [doAction(ServerSettings, {P, X, Y}, Action) | PList], {actions, Player, Action});
doActions(SS, [P | T], PList, Action) ->
    doActions(SS, T, [P | PList], Action).
    
doAction({PlayerMoveSpeed, _Collision}, {P, X, Y}, move_up) ->
    {P, X, Y+PlayerMoveSpeed};
doAction({PlayerMoveSpeed, _Collision}, {P, X, Y}, move_down) ->
    {P, X, Y-PlayerMoveSpeed};
doAction({PlayerMoveSpeed, _Collision}, {P, X, Y}, move_left) ->
    {P, X-PlayerMoveSpeed, Y};
doAction({PlayerMoveSpeed, _Collision}, {P, X, Y}, move_right) ->
    {P, X+PlayerMoveSpeed, Y}.

state_sender() ->
    receive 
        {new_state, NewState} ->
            io:format("~w~n", [NewState]),
            state_sender();
	{terminate, Reason} ->
	    io:format("~w~n", [Reason]);
        E ->
            io:format("~w~n", [E]),
            state_sender()
    end.


add(Pname, Action) ->
    fun() ->
	    case mnesia:read({actions, Pname}) of
		[] ->
		    %% no action, add it
		    Entry = #actions{player_name = Pname,action = Action},
		    mnesia:write(Entry),
                    ok;
		[E] ->
                    %% update action
                    E1 = E#actions{action = Action},
                    mnesia:write(E1),
                    ok
	    end
    end.

remove(Pname) ->
    fun() ->
	    case mnesia:read({actions, Pname}) of
		[] ->
		    %% no server
		    {error, no_such_player};
		[_E] ->
                    mnesia:delete({server, Pname})
            end
    end.


available() ->
    fun() ->
            mnesia:foldl(fun(X,XS)-> [X|XS]
                        end, 
                        [], 
                        actions)
    end.




send_term(Socket, Term) ->
    gen_tcp:send(Socket, [term_to_binary(Term)]).

do_call(C) ->
    Fun = the_func(C),
    mnesia:transaction(Fun).

the_func({add_player, PlayerName, Ip}) -> udp_loop:addPlayer(PlayerName, Ip);


the_func({a_add, Player_Name, Action})  ->  tcp_udp_server:add(Player_Name, Action);
the_func({a_remove, Player_Name})  ->  tcp_udp_server:remove(Player_Name);
the_func({a_available})  ->  tcp_udp_server:available();

the_func({add, Server_Name, Ip})  ->  bank:add(Server_Name, Ip);
the_func({remove, Server_Name}) ->  bank:remove(Server_Name);
the_func({available}) ->  bank:available();
the_func({clear}) ->  bank:clear();
the_func({ping, Server_Name}) -> bank:ping(Server_Name);
the_func({addPlayer, PlayerName}) -> game_logic:addPlayer(PlayerName);
the_func({getPos, PlayerName}) -> game_logic:getPos(PlayerName);
the_func({getAllPos}) -> game_logic:getAllPos();
the_func({move, PlayerName, Direction, Amount}) -> game_logic:move(PlayerName, Direction, Amount).

state_generator(ServerSettings, N) -> 
    PlayerList = [{lists:append("Player",(lists:flatten(io_lib:format("~p", [X])))),5,5} || X <- lists:seq(1,N)],
    {ServerSettings, PlayerList}.

action_generator(N) ->
    ActionList = [{lists:append("Player",(lists:flatten(io_lib:format("~p", [X])))),move_right} || X <- lists:seq(1,N)],
    action_generator(ActionList, durp).

action_generator([], durp) ->
    ok;
action_generator([X|XS], durp) ->
    {Player_Name, Action} = X,
    do_call({a_add, Player_Name, Action}),
    action_generator(XS, durp).
