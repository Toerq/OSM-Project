-module(test).
-compile(export_all).

%% should get a action.hrl file
-record(action, {player_id, action, varlist}).

%% ServerSettings = {VelFactor, GridLimit, VelLimit, Friction} VelFactor must be greater than Friction
%% Player = {NameString, Pos, Vel, Hp, Id} where pos and vel are tuples of {x,y}
%% State = {ServerSettings, PlayerList}

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(action, 
			[{disc_copies,[node()]},
			 {attributes, 
			  record_info(fields, action)}]),
    mnesia:stop().

start() ->
    mnesia:start().

game_tick_once(State) ->
    Time = erlang:now(),
    Actions = get_actions(), %% mnesia:foldl(fun(X,XS) -> [X|XS] end, [], actions), %% Fetch actions
    NewState = doActions(State, Actions),
    {ok, timer:now_diff(erlang:now(), Time), NewState}.  

game_state(_Tick, _StateSender, State, 0) ->
    State;
game_state(Tick, StateSender, State, N) ->
    Time = erlang:now(),
    Actions = get_actions(),
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
    doActions({ServerSettings, doActionsAux(ServerSettings, PlayerList, [], A)}, T).

doActionsAux(_ServerSettings, [], PList, _Action)->
    PList;
doActionsAux(ServerSettings, [{Name, Pos, Vel, Hp, Id} | T], PList, {PlayerId, Action, _}) when Id =:= PlayerId ->
    doActionsAux(ServerSettings, T, [doAction(ServerSettings, {Name, Pos, Vel, Hp, Id}, Action) | PList], {PlayerId, Action});
doActionsAux(ServerSettings, [{Name, Pos, Vel, Hp, Id} | T], PList, {PlayerId, Action, _}) when PlayerId =:= global ->
    doActionsAux(ServerSettings, T, [doAction(ServerSettings, {Name, Pos, Vel, Hp, Id}, Action) | PList], {PlayerId, Action});
doActionsAux(SS, [P | T], PList, Action) ->
    doActionsAux(SS, T, [P | PList], Action).

doAction({PlayerMoveFactor, GridLimit, VelLimit, Friction}, {NameString, Pos, Vel, Hp, Id}, Action) ->
    case Action of
	move_up ->
	    {{NXpos, NYpos}, {NXVel, NYVel}} = posChange(Pos, Vel, {0,PlayerMoveFactor}, VelLimit, GridLimit, Friction),
	    {NameString, {NXpos, NYpos}, {NXVel, NYVel}, Hp, Id};
	move_down ->
	    {{NXpos, NYpos}, {NXVel, NYVel}} = posChange(Pos, Vel, {0,-PlayerMoveFactor}, VelLimit, GridLimit, Friction),
	    {NameString, {NXpos, NYpos}, {NXVel, NYVel}, Hp, Id};
	move_left ->
	    {{NXpos, NYpos}, {NXVel, NYVel}} = posChange(Pos, Vel, {-PlayerMoveFactor,0}, VelLimit, GridLimit, Friction),
	    {NameString, {NXpos, NYpos}, {NXVel, NYVel}, Hp, Id};
	move_right ->
	    {{NXpos, NYpos}, {NXVel, NYVel}} = posChange(Pos, Vel, {PlayerMoveFactor,0}, VelLimit, GridLimit, Friction),
	    {NameString, {NXpos, NYpos}, {NXVel, NYVel}, Hp, Id};
	stop ->
	    {{NXpos, NYpos}, {NXVel, NYVel}} = posChange(Pos, Vel, {0,0}, VelLimit, GridLimit, Friction),
	    {NameString, {NXpos, NYpos}, {NXVel, NYVel}, Hp, Id}
    end.

posChange({Xpos, Ypos}, {XVel, YVel}, {Xc, Yc}, VelLimit, GridLimit, Friction) ->
    NXVel = limitor(XVel + Xc, VelLimit, Friction),
    NYVel = limitor(YVel + Yc, VelLimit, Friction),
    NXpos = modulor(Xpos + NXVel, GridLimit),
    NYpos = modulor(Ypos + NYVel, GridLimit),
    {{NXpos, NYpos}, {NXVel, NYVel}}.

limitor(X, Limit, Fric) ->
    if X > Limit ->
	    Limit;
       X < -Limit ->
	    -Limit;
       X > 0 ->
	    if (X - Fric) > 0 ->
		    X - Fric;
	       true ->
		    0
	    end;
       true ->
	    if (X + Fric) > 0 ->
		    0;
	       true ->
		    X + Fric
	    end
    end.

modulor(X, Mod) ->
    if X > Mod ->
	    X - Mod - 1;
       X < 0 ->
	    Mod + X + 1;
       true ->
	    X
    end.

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

state_generator(ServerSettings, N) -> 
    PlayerList = [{lists:append("Player",(lists:flatten(io_lib:format("~p", [X])))), {5,5}, {6,6}, 100, X} || X <- lists:seq(1,N)],
    {ServerSettings, PlayerList}.

action_generator(N) ->
    ActionList = [{X, move_right, []} || X <- lists:seq(1,N)],
    action_generator(ActionList, durp).

action_generator([], durp) ->
    ok;
action_generator([X|XS], durp) ->
    {Player_Id, Action, VarList} = X,
    do_call({a_add, Player_Id, Action, VarList}),
    action_generator(XS, durp).



do_call(C) ->
    Fun = the_func(C),
    mnesia:transaction(Fun).

%% -record(action, {player_id, action, varlist}).

add(PId, Action, VarList) ->
    fun() ->
	    case mnesia:read({action, PId}) of
		[] ->
		    %% no action, add it
		    Entry = #action{player_id = PId, action = Action, varlist = VarList},
		    mnesia:write(Entry),
                    ok;
		[E] ->
                    %% update action
                    E1 = E#action{action = Action, varlist = VarList},
                    mnesia:write(E1),
                    ok
	    end
    end.

remove(PId) ->
    fun() ->
	    case mnesia:read({actions, PId}) of
		[] ->
		    %% player id not found
		    {error, no_such_player};
		[_E] ->
                    mnesia:delete({server, PId})
            end
    end.


available() ->
    fun() ->
            mnesia:foldl(fun(X,XS)-> [X|XS]
                        end, 
                        [], 
                        action)
    end.

clear() ->
    fun() ->
	    mnesia:clear_table(action)
    end.

get_actions() ->
    {_, AList} = ?MODULE:do_call({a_available}),
    [{X,Y,Z} ||{_, X, Y, Z} <- AList].

the_func({a_add, Player_Id, Action, VarList})  ->  ?MODULE:add(Player_Id, Action, VarList);
the_func({a_remove, Player_Id})  ->  ?MODULE:remove(Player_Id);
the_func({a_clear})  ->  ?MODULE:clear();
the_func({a_available})  ->  ?MODULE:available().

%% ServerSettings = {VelFactor, GridLimit, VelLimit, Friction} VelFactor must be greater than Friction
%% Player = {NameString, Pos, Vel, Hp, Id} where pos and vel are tuples of {x,y}
%% State = {ServerSettings, PlayerList}


test(N) ->
    test:init(),
    test:start(),
    test:action_generator(N),
    SS = {4, 1000, 21, 1},
    State = test:state_generator(SS, N),
    StateSender = spawn(fun() -> test:state_sender() end),
    test:game_state(30, StateSender, State, 30),
    StateSender ! {terminate, "Test Done"},
    mnesia:stop(),
    ok.


%% test:game_tick_once(State).
