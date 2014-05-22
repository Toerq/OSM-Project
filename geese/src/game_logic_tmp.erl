-module(game_logic_tmp).
-export([do_actions/2, make_new_state/0]).

-define(PLAYERHEIGHT, 25).
-define(PLAYERWIDTH, 10).


%% STATE {SERVERSETTINGS, ENTITYLIST}
%% SERVERSETINGS:
%% {move_factor,
%%  gravity_factor,
%%  air_friction,
%%  base_jump_factor,
%%  grid_limit,
%%  vel_limit,
%%  level_list}

make_new_state() ->
    {{10.0, 
      5.0, 
      1.0, 
      15.0, 
      {500, 500}, 
      17.0, 
      [{{0, 0},{500, 10}}, {{0, 0},{10, 500}}, {{490, 0},{500, 500}}]}, 
     {[],[]}}.

%% do_actions(STATE, ACTIONLIST)

do_actions({Server_settings, Entity_lists}, Action_list) ->
    iterate_state(apply_actions(Server_settings, Entity_lists, Action_list)).

apply_actions(Server_settings, Entity_lists, []) ->
    {Server_settings, Entity_lists};
apply_actions(Server_settings, {Player_list, Bullet_list}, [{Entity_id, Action, Var_list} | T]) when Action =:= fire ->
    [Type, Direction, Pos] = Var_list,
    New_bullet_list = [{Entity_id, Type, Pos, Direction} | Bullet_list],
    apply_actions(Server_settings, {Player_list, New_bullet_list}, T);
apply_actions(Server_settings, {Player_list, Bullet_list}, [A | T]) ->
    apply_actions(Server_settings, 
		  {apply_action(Server_settings, Player_list, A, []), Bullet_list}, 
		  T).

apply_action(_Server_settings, [], _A, Aux_list) ->
    Aux_list;
apply_action(Server_settings, [{Name, Pos, Vel, Hp, E_id} | T], {Entity_id, Action, Var_list}, Aux_list) when E_id =:= Entity_id ->
    lists:append([[apply_action_aux(Server_settings, 
				    {Name, Pos, Vel, Hp, E_id}, 
				    Action, 
				    Var_list) | T], Aux_list]);
apply_action(Server_settings, [E | T], A, Aux_list) ->
    apply_action(Server_settings, T, A, [E | Aux_list]).

apply_action_aux(Server_settings, {Name, Pos, Vel, Hp, E_id}, Action, Var_list) ->
    case Action of
	move ->
	    [Direction] = Var_list,
	    move(Server_settings, {Name, Pos, Vel, Hp, E_id}, Direction);
	jump ->
	    [Type] = Var_list,
	    jump(Server_settings, {Name, Pos, Vel, Hp, E_id}, Type)
    end.

move(Server_settings, {Name, Pos, Vel, Hp, E_id}, Direction) ->
    {Move_factor,
     Gravity_factor,
     Air_friction,
     _Base_jump_factor,
     _Grid_limit,
     Vel_limit,
     _Level_list} = Server_settings,
    case Direction of 
	right ->
	    Dir_vel = Move_factor;
	left ->
	    Dir_vel = -Move_factor
    end,
    {X_vel, Y_vel} = Vel,
    New_x_vel = limitor(X_vel+Dir_vel, Vel_limit, Air_friction),
    {Name, Pos, {New_x_vel, Y_vel-Gravity_factor}, Hp, E_id}.

jump(Server_settings, {Name, Pos, Vel, Hp, E_id}, Type) ->
    {_Move_factor,
     Gravity_factor,
     Air_friction,
     Base_jump_factor,
     _Grid_limit,
     Vel_limit,
     Level_list} = Server_settings,
    {X_vel, Y_vel} = Vel,
    case can_jump(Pos, Level_list) of
	true ->
	    case Type of 
		weak ->
		    Jump_vel = Base_jump_factor;
		normal ->
		    Jump_vel = Base_jump_factor*2;
		strong ->
		    Jump_vel = Base_jump_factor*3
	    end,
	    {Name, Pos, {limitor(X_vel, Vel_limit, Air_friction), Y_vel+Jump_vel-Gravity_factor}, Hp, E_id};
	false ->
	    {Name, Pos, Vel, Hp, E_id}
    end.

can_jump(_Pos, []) ->
    false;
can_jump({X, Y}, [O | T]) ->
    {{X_start, X_end}, Y_end} = ground_interval(O),
    if Y =:= Y_end ->
	    if X >= X_start andalso X =< X_end ->
		    true;
	       true ->
		    can_jump({X, Y}, T)
	    end;
       true ->
	    can_jump({X, Y}, T)
    end.

ground_interval({{X_start, _Y_start}, {X_end, Y_end}}) ->
    {{X_start, X_end},Y_end}.
    
iterate_state(State) ->
    iterate_state_aux(State, []).

iterate_state_aux({Server_settings, {[], []}}, Aux_list) ->
    {Server_settings, {Aux_list,[]}};
iterate_state_aux({Server_settings, {[P | Player_list], []}}, Aux_list) ->
    iterate_state_aux({Server_settings, {Player_list, []}}, [iterate_player(Server_settings, P) | Aux_list]);
iterate_state_aux({Server_settings, {Player_list, [B | Bullet_list]}}, Aux_list) ->
    iterate_state_aux({Server_settings, {iterate_bullet(Server_settings, Player_list, B), Bullet_list}}, Aux_list).

iterate_player(Server_settings, Player) ->    
    {Name, Pos, Vel, Hp, Id} = Player,
    {_Move_factor,
     _Gravity_factor,
     _Air_friction,
     _Base_jump_factor,
     Grid_limit,
     Vel_limit,
     Level_list} = Server_settings,
    {New_vel, New_pos, New_hp} = iterate_move(Vel, Pos, Hp, Level_list), 
    {Name, New_pos, New_vel, New_hp, Id}.

iterate_move(Vel, Pos, Hp, Level_List) ->
    {Vertical_list, Horizontal_list} = get_borders(Level_list, {[],[]}),
    tbi.

get_borders([], Aux) ->
    Aux;
get_borders([O| T], {Aux_v, Aux_h}) ->
    tbi.


iterate_bullet(Server_settings, Player_list, Bullet) ->
    Player_list.


    
    
    







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
 	    X - Mod;
       X < 0 ->
 	    Mod + X;
       true ->
 	    X
    end.
