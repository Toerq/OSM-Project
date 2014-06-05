%% @author Johan Gille
-module(game_state).
-export([state/4, start/3, register_action/1, state_sender/1]).

%% @doc Starts a new state loop with a given tickrate and a table name.
%% Starts a mnesia database with Db_name as table name, 
%% also starts a state with given Tick_rate that will send its state to
%% State_sender. Returns {ok, Pid} where Pid is the Pid of the state process.
%% === Example ===
%% <div class="example">
%% start(table_1, PID1, 32).
%% {ok, PID2}
%% </div>
-spec start(D::atom(), S::pid(), T::integer()) -> {atom(), pid()}.

start(Db_name, State_sender, Tick_rate) ->
    action_db:init(Db_name),
    State = game_logic:make_new_state(),
    Pid = spawn(fun() ->
			state(Tick_rate, State_sender, Db_name, State)
		end),
    {ok, Pid}.

%% @doc Will register a action into an mnesia database.
%% return value varies depending on action.
%% === Example ===
%% <div class="example">
%% register_action({action_add, table_1, 4, move_up, []}).
%% </div>
-spec register_action(Action::tuple()) -> ok.

register_action(Action) ->
    E =  action_db:do_call(Action),
    io:format("Do call: ~w~n", [E]),
    E.
    
%% @doc State will loop and read actions so it can update its State. Depending on what actions
%% it will read. It will calculate a newstate and send it at a given tickrate.
%% The new state will be sent to the State_sender and the action will be 
%% read from the mnesia database with table Db_name.
-spec state(Tick::integer(), State_s::pid(), Db_n::atom(), State::tuple()) -> ok.

state(Tick, State_sender, Db_name, State) ->
    Time = erlang:now(),
    Actions = action_db:get_actions(Db_name),
    {New_state, Bullet_list} = game_logic:do_actions(State, Actions),
    {Server_settings, {Player_list, _Bullet_list}} = New_state,
    {_mf, _gf, _af, _bjf, _gl, _vl, Level_list} = Server_settings,
    remove_server_action(Db_name),
    State_lists = {Player_list, Bullet_list, Level_list},
    State_sender ! {new_state, round_state(State_lists)},
    Sleep_time = ((1000000 div Tick) - 
		     timer:now_diff(erlang:now(), Time))div 1000,  
    if Sleep_time > 0 ->
	    timer:sleep(Sleep_time);
       true ->
	    %% no sleep
	    ok 
    end,
    state(Tick, State_sender, Db_name, New_state).

round_state({Player_list, Bullet_list, Level_list}) ->
    {round_players(Player_list,[]), round_bullets(Bullet_list, []), Level_list}.

round_players([], Aux) ->
    Aux;
round_players([P | Player_list], Aux) ->
    {Name, {X, Y}, {X_vel, Y_vel}, Hp, Power, Score, E_id} = P,
    round_players(Player_list, [{Name, {round(X), round(Y)}, {round(X_vel), round(Y_vel)}, Hp, Power, Score, E_id} | Aux]).

round_bullets([], Aux) ->
    Aux;
round_bullets([B | Bullet_list], Aux) ->
    {Id, {X0, Y0}, {X1,Y1}} = B,
    round_bullets(Bullet_list, [{Id, {round(X0), round(Y0)}, {round(X1), round(Y1)}} | Aux]). 

%% @doc Will loop and update and send out its State. 
%% state_sender will loop a given State and update it when it 
%% receives a new state and send its current sate if someone asks for it.
-spec state_sender(State::tuple()) -> ok.
state_sender(State) ->
    receive 
        {new_state, NewState} ->
	    state_sender(NewState);
	{terminate, Reason} ->
	    io:format("~s~n", [Reason]);
	{get_state, PID} ->
	    PID ! {state, State},
	    state_sender(State);
        E ->
            io:format("~w~n", [E]),
            state_sender(State)
    end.

remove_server_action(Db_name) ->
    action_db:do_call({action_remove, Db_name, server}).
