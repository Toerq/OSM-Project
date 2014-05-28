-module(game_logic).
-compile(export_all).
-export([do_actions/2, make_new_state/0]).

-define(PLAYERHEIGHT, 35).
-define(PLAYERWIDTH, 13).
-define(PLAYERLEGSHEIGHT, 12).
-define(PLAYERBODYHEIGHT, 13).
-define(PLAYERHEADHEIGHT, 10).
-define(PLAYERMIDDLEX, 7).
-define(PLAYERMIDDLEY, 20).

-define(DAMAGELEGS, 1).
-define(DAMAGEBODY, 2).
-define(DAMAGEHEAD, 4).

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
    {{3.7, 
      1.8, 
      1.8, 
      21.0, 
      {500, 500}, 
      12.6, 
      [{{0,0},{1000,1000}},{{250,100},{400,140}},{{500,200},{650,240}},{{-10,-10},{810,5}},{{100,-40},{140,50}}]}, 
     {[],[]}}.

%% do_actions(STATE, ACTIONLIST)
%% {State, Bullet_list}
do_actions({Server_settings, Entity_lists}, Action_list) ->
    iterate_state(apply_actions(Server_settings, Entity_lists, Action_list)).

apply_actions(Server_settings, Entity_lists, []) ->
    {Server_settings, Entity_lists};
apply_actions(Server_settings, {Player_list, Bullet_list}, [{_Entity_id, Action, Var_list} | T]) when Action =:= server ->
    [Type, Argument] = Var_list,
    {New_server_settings, New_player_list, New_bullet_list} = 
	apply_server_action(Server_settings, Player_list, Bullet_list, Type, Argument),
    apply_actions(New_server_settings, {New_player_list, New_bullet_list}, T);
apply_actions(Server_settings, {Player_list, Bullet_list}, [{Entity_id, Action, Var_list} | T]) when Action =:= fire ->
    [Type, Direction] = Var_list,
    New_bullet_list = [{Entity_id, Type, Direction} | Bullet_list],
    apply_actions(Server_settings, {Player_list, New_bullet_list}, T);
apply_actions(Server_settings, {Player_list, Bullet_list}, [A | T]) ->
    apply_actions(Server_settings, 
		  {apply_action(Server_settings, Player_list, A, []), Bullet_list}, 
		  T).

apply_server_action(Server_settings, Player_list, Bullet_list, Type, Argument) ->
    case Type of
	add_player ->
	    New_player_list = add_player(Argument, Player_list, []),
	    {Server_settings, New_player_list, Bullet_list};
	change_settings ->
	    New_server_settings = change_settings(Server_settings, Argument),
	    {New_server_settings, Player_list, Bullet_list};
	remove_player ->
	    New_player_list = remove_player(Argument, Player_list, []),
	    {Server_settings, New_player_list, Bullet_list}
    end.



apply_action(_Server_settings, [], _A, Aux_list) ->
    Aux_list;
apply_action(Server_settings, [{Name, Pos, Vel, Hp, Power, E_id} | T], {Entity_id, Action, Var_list}, Aux_list) when E_id =:= Entity_id ->
    lists:append([[apply_action_aux(Server_settings, 
				    {Name, Pos, Vel, Hp, Power, E_id}, 
				    Action, 
				    Var_list) | T], Aux_list]);
apply_action(Server_settings, [E | T], A, Aux_list) ->
    apply_action(Server_settings, T, A, [E | Aux_list]).

apply_action_aux(Server_settings, {Name, Pos, Vel, Hp, Power, E_id}, Action, Var_list) ->
    case Action of
	move ->
	    [Direction] = Var_list,
	    move(Server_settings, {Name, Pos, Vel, Hp, Power, E_id}, Direction);
	jump ->
	    [Type] = Var_list,
	    jump(Server_settings, {Name, Pos, Vel, Hp, Power, E_id}, Type)
    end.

move(Server_settings, {Name, Pos, Vel, Hp, Power, E_id}, Direction) ->
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
	    Dir_vel = -Move_factor;
	stop ->
	    Dir_vel = 0.0
    end,
    {X_vel, Y_vel} = Vel,
    New_x_vel = limitor(X_vel+Dir_vel, Vel_limit, Air_friction),
    {Name, Pos, {New_x_vel, Y_vel-Gravity_factor}, Hp, Power, E_id}.

jump(Server_settings, {Name, Pos, Vel, Hp, Power, E_id}, Type) ->
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
	    {Name, Pos, {limitor(X_vel, Vel_limit, Air_friction), Y_vel+Jump_vel-Gravity_factor}, Hp, Power, E_id};
	false ->
	    {Name, Pos, {limitor(X_vel, Vel_limit, Air_friction), Y_vel-Gravity_factor}, Hp, Power, E_id}
    end.

can_jump(_Pos, []) ->
    false;
can_jump({X, Y}, [O | T]) ->
    {{X_start, X_end}, Y_end} = ground_interval(O),
    %% io:format("Need to be at: ~w~n Is at: ~w~n",[Y_end, Y]),
    if round(Y) =:= Y_end+1 -> %% Fraud x2
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


add_player(Player, [], Aux_list) ->
    [Player | Aux_list];
add_player({Name, Pos, Vel, Hp, Power, Id}, [{N, P, V, H, PW, I} | T], Aux_list) when I =/= Id ->
    add_player({Name, Pos, Vel, Hp, Power, Id}, T, [{N, P, V, H, PW, I} | Aux_list]);
add_player(_ , Players, Aux_list) ->
    lists:append([Players, Aux_list]).

remove_player(_Player, [], Aux_list) ->
    Aux_list;
%%remove_player(Player, [P | T], Aux_list) when P =:= Player ->
remove_player({_Name, _Pos, _Vel, _Hp, _Power, Id}, [{_N, _P, _V, _H, I} | T], Aux_list) when I =:= Id ->
    lists:append([T, Aux_list]);
remove_player(Player, [P | T], Aux_list) ->
    remove_player(Player, T, [P | Aux_list]).


change_settings({M_f, G_f, A_f, B_j, G_l, V_l, L_l}, New_settings) ->
    Old_settings = [M_f, G_f, A_f, B_j, G_l, V_l, L_l],
    [N_m_f, N_g_f, N_a_f, N_b_j, N_g_l, N_v_l, N_l_l] = settings_update(Old_settings, New_settings, []),
    {N_m_f, N_g_f, N_a_f, N_b_j, N_g_l, N_v_l, N_l_l}.

settings_update([],[], Aux) ->
    lists:reverse(Aux);
settings_update([O | OT], [N | NT], Aux) when N =:= no_change ->
    settings_update(OT, NT, [O | Aux]);
settings_update([_O | OT], [N | NT], Aux) ->
    settings_update(OT, NT, [N | Aux]).


iterate_state({Server_settings, {Player_list, Bullet_list}}) ->
    {State, Bullet_info_list} = iterate_state_aux({Server_settings, {Player_list, Bullet_list}}, [], []),
    {State, Bullet_info_list}.

iterate_state_aux({Server_settings, {[], []}}, Aux_list, Bullet_info_list) ->
    {{Server_settings, {Aux_list,[]}}, Bullet_info_list};
iterate_state_aux({Server_settings, {[P | Player_list], []}}, Aux_list, Bullet_info_list) ->
    iterate_state_aux({Server_settings, {Player_list, []}}, [iterate_player(Server_settings, P) | Aux_list], Bullet_info_list);
iterate_state_aux({Server_settings, {Player_list, [B | Bullet_list]}}, Aux_list, Bullet_info_list) ->
    {New_player_list, Bullet_info} = iterate_bullet(Server_settings, Player_list, B),
    if Bullet_info =:= nope ->
	    iterate_state_aux({Server_settings, {New_player_list, Bullet_list}}, Aux_list, Bullet_info_list);
       true ->
	    iterate_state_aux({Server_settings, {New_player_list, Bullet_list}}, Aux_list, [Bullet_info |Bullet_info_list])
    end.

iterate_player(Server_settings, Player) ->    
    {Name, Pos, Vel, Hp, Power, Id} = Player,
    {_Move_factor,
     _Gravity_factor,
     _Air_friction,
     _Base_jump_factor,
     _Grid_limit,
     _Vel_limit,
     Level_list} = Server_settings,
    {New_vel, New_pos, New_hp} = iterate_move(Vel, Pos, Hp, Level_list),
    New_power = new_power(Power, New_vel),
    {Name, New_pos, New_vel, New_hp, New_power, Id}.

get_borders([], Aux) ->
    Aux;
get_borders([O| T], {Aux_v, Aux_h}) ->
    {{X_start,Y_start},{X_end, Y_end}} = O,
    Left_v = {X_start, {Y_start, Y_end}}, 
    Right_v = {X_end, {Y_start, Y_end}},
    Top_h = {{X_start, X_end}, Y_end},
    Bot_h = {{X_start, X_end}, Y_start},
    get_borders(T, {[Left_v |[ Right_v| Aux_v]] , [Top_h |[Bot_h | Aux_h]]}).

%% Vel = {4, -5}.
%% Pos = {3,3}.
%% Hp = 100.
%% Level_list = [{{0,0},{10,10}}].

collision([],[],[],[], Aux) ->
    Aux;
collision([Origin | O],[Short | S],[Ideal | I],[Type | T], Aux) ->
    if Short =:= Ideal ->
	    collision(O, S, I, T, Aux);
       true ->
	    if Aux =:= none ->
		    collision(O, S, I, T, {Origin, Short, Type});
	       true ->
		    {Aux_o, Aux_s, Aux_t} = Aux,
		    collision(O,S,I,T, shortest_point(Aux_o, Aux_s, Origin, Short, Aux_t, Type))
	    end
    end.

shortest_point({X_0,Y_0},{X_1,Y_1},{X_2,Y_2},{X_3,Y_3}, T_0, T_2) ->
    Distance1 = math:sqrt( (X_1 - X_0)*(X_1 - X_0) + (Y_1 - Y_0)*(Y_1 - Y_0)),
    Distance2 = math:sqrt( (X_3 - X_2)*(X_3 - X_2) + (Y_3 - Y_2)*(Y_3 - Y_2)),
    if Distance1 > Distance2 ->
	    {{X_2,Y_2},{X_3,Y_3},T_2};
       true ->
	    {{X_0,Y_0},{X_1,Y_1},T_0}
    end.

iterate_move(Vel, Pos, Hp, Level_list) ->
    io:format("~nFall: 0 ~w~n~w~n", [Vel, Pos]),
    {Vertical_list, Horizontal_list} = get_borders(Level_list, {[],[]}),
    {X_vel, Y_vel} = Vel,
    %% Player got 4 points, 1:bot-left, 2:bot-right, 3:top-left, 4:top-right
    {X_1, Y_1} = Pos,  
    {X_2, Y_2} = {X_1 + ?PLAYERWIDTH, Y_1},
    {X_3, Y_3} = {X_1, Y_1 + ?PLAYERHEIGHT},
    {X_4, Y_4} = {X_1 + ?PLAYERWIDTH, Y_1 + ?PLAYERHEIGHT},
    Ideal_point_1 = {X_1+X_vel, Y_1+Y_vel},
    Ideal_point_2 = {X_2+X_vel, Y_2+Y_vel},
    Ideal_point_3 = {X_3+X_vel, Y_3+Y_vel},
    Ideal_point_4 = {X_4+X_vel, Y_4+Y_vel},
    Line_1 = make_line({X_1,Y_1}, Ideal_point_1),
    Line_2 = make_line({X_2,Y_2}, Ideal_point_2),
    Line_3 = make_line({X_3,Y_3}, Ideal_point_3),
    Line_4 = make_line({X_4,Y_4}, Ideal_point_4),
    Dummy_value_v = {{-99999,{-99999,-99999}},{-99999,-99999}, ver},
    Dummy_value_h = {{{-99999,-99999},-99999},{-99999,-99999}, hor},
    {_Border_hit_1, Point_1, Type_1} = border_hit(Line_1, Vertical_list, Horizontal_list, Dummy_value_v, Dummy_value_h),
    {_Border_hit_2, Point_2, Type_2} = border_hit(Line_2, Vertical_list, Horizontal_list, Dummy_value_v, Dummy_value_h),
    {_Border_hit_3, Point_3, Type_3} = border_hit(Line_3, Vertical_list, Horizontal_list, Dummy_value_v, Dummy_value_h),
    {_Border_hit_4, Point_4, Type_4} = border_hit(Line_4, Vertical_list, Horizontal_list, Dummy_value_v, Dummy_value_h),
    Short_1 = shortest_distance({X_1,Y_1}, Ideal_point_1, Point_1),
    Short_2 = shortest_distance({X_2,Y_2}, Ideal_point_2, Point_2),
    Short_3 = shortest_distance({X_3,Y_3}, Ideal_point_3, Point_3),
    Short_4 = shortest_distance({X_4,Y_4}, Ideal_point_4, Point_4),
    case collision([{X_1, Y_1},{X_2, Y_2},{X_3, Y_3},{X_4, Y_4}],
		   [Short_1,Short_2,Short_3,Short_4],
		   [Ideal_point_1,Ideal_point_2,Ideal_point_3,Ideal_point_4], 
		   [Type_1, Type_2, Type_3, Type_4], none) of
	none ->
	    %% Bra, ingen krock.
	    %% {Vel, Pos, Hp}
	    io:format("~n Fall: 1~n"),
	    {{X_vel, Y_vel},Ideal_point_1, Hp};
	{Origin, Short, Type} ->
	    io:format("~n Fall: 2~n"),
	    {Origin_x, Origin_y} = Origin,
	    {Diff_x, Diff_y} = {X_1 - Origin_x, Y_1 - Origin_y},
	    {Short_x, Short_y} = Short,
	    {New_short_x, New_short_y} = {Short_x + Diff_x, Short_y + Diff_y}, 
	    io:format("~nOJOJ~n~w~n~w~n~w~n~w~n~w~n", [Origin, Short, Type, New_short_x, New_short_y]),
	    case Type of 
		hor ->
		    io:format("~n Fall: 3~n"),
		    %% your new pos will be Short() and you will lose your vertical vel,
		    %% check from Short() to new pos with your remaining hor. vel
		    %%{X_short, Y_short} = Short,
		    {New_x_vel, New_y_vel} = {X_vel - (New_short_x - X_1), 0.0},
		    A = {Snd_x_1, Snd_y_1} = {New_short_x, New_short_y},  
		    B = {Snd_x_2, Snd_y_2} = {New_short_x + ?PLAYERWIDTH, New_short_y},
		    C = {Snd_x_3, Snd_y_3} = {New_short_x, New_short_y + ?PLAYERHEIGHT},
		    D = {Snd_x_4, Snd_y_4} = {New_short_x + ?PLAYERWIDTH, New_short_y + ?PLAYERHEIGHT},
		    Snd_ideal_point_1 = {Snd_x_1 + New_x_vel, Snd_y_1},
		    Snd_ideal_point_2 = {Snd_x_2 + New_x_vel, Snd_y_2},
		    Snd_ideal_point_3 = {Snd_x_3 + New_x_vel, Snd_y_3},
		    Snd_ideal_point_4 = {Snd_x_4 + New_x_vel, Snd_y_4},
		    Snd_line_1 = make_line({Snd_x_1, Snd_y_1}, Snd_ideal_point_1),		    
		    Snd_line_2 = make_line({Snd_x_2, Snd_y_2}, Snd_ideal_point_2),		    
		    Snd_line_3 = make_line({Snd_x_3, Snd_y_3}, Snd_ideal_point_3),
		    Snd_line_4 = make_line({Snd_x_4, Snd_y_4}, Snd_ideal_point_4),
		    {_Border_1, Snd_point_1, _Type_1} = border_hit(Snd_line_1, Vertical_list, [], Dummy_value_v, Dummy_value_h),
		    {_Border_2, Snd_point_2, _Type_2} = border_hit(Snd_line_2, Vertical_list, [], Dummy_value_v, Dummy_value_h),
		    {_Border_3, Snd_point_3, _Type_3} = border_hit(Snd_line_3, Vertical_list, [], Dummy_value_v, Dummy_value_h),
		    {_Border_4, Snd_point_4, _Type_4} = border_hit(Snd_line_4, Vertical_list, [], Dummy_value_v, Dummy_value_h),
		    Snd_short_1 = shortest_distance({Snd_x_1, Snd_y_1}, Snd_ideal_point_1, Snd_point_1),
		    Snd_short_2 = shortest_distance({Snd_x_2, Snd_y_2}, Snd_ideal_point_2, Snd_point_2),
		    Snd_short_3 = shortest_distance({Snd_x_3, Snd_y_3}, Snd_ideal_point_3, Snd_point_3),
		    Snd_short_4 = shortest_distance({Snd_x_4, Snd_y_4}, Snd_ideal_point_4, Snd_point_4),
		    io:format("~nPos1: ~w~nPos2: ~w~nPos3: ~w~nPos4: ~w~n", [A,B,C,D]),
		    io:format("~nShort1: ~w~nShort2: ~w~nShort3: ~w~nShort4: ~w~nShortest: ~w~n", 
		      	      [Snd_short_1,Snd_short_2,Snd_short_3,Snd_short_4, collision([{Snd_x_1, Snd_y_1},{Snd_x_2, Snd_y_2},
		      									   {Snd_x_3, Snd_y_3},{Snd_x_4, Snd_y_4}],
		      									  [Snd_short_1,Snd_short_2,Snd_short_3,Snd_short_4],
		      									  [Snd_ideal_point_1,Snd_ideal_point_2,
		      									   Snd_ideal_point_3,Snd_ideal_point_4], 
		      									  [Type_1, Type_2, Type_3, Type_4], none)]),
		    
		    case collision([{Snd_x_1, Snd_y_1},{Snd_x_2, Snd_y_2},{Snd_x_3, Snd_y_3},{Snd_x_4, Snd_y_4}],
				   [Snd_short_1,Snd_short_2,Snd_short_3,Snd_short_4],
				   [Snd_ideal_point_1,Snd_ideal_point_2,Snd_ideal_point_3,Snd_ideal_point_4], 
				   [Type_1, Type_2, Type_3, Type_4], none) of
			none ->
			    io:format("~n Fall: 4~n"),
			    %% Bra, ingen krock.
			    %% {Vel, Pos, Hp}
			    {{New_x_vel, New_y_vel}, Snd_ideal_point_1, Hp};
			{Snd_origin, Snd_short, _Type} ->
			    io:format("~n Fall: 5~n"),
			    %% Bra, krock!
			    {Snd_origin_x, Snd_origin_y} = Snd_origin,
			    {Snd_diff_x, Snd_diff_y} = {Snd_x_1 - Snd_origin_x, Snd_y_1 - Snd_origin_y},
			    {Snd_short_x, Snd_short_y} = Snd_short,
			    Snd_new_short = {Snd_short_x + Snd_diff_x, Snd_short_y + Snd_diff_y}, 
			    %% io:format("~new_y_vel: ~w~nnew_short: ~w~n", [New_y_vel,Snd_new_short]),
			    %% io:format("~nkolla här!!~n"),
			    {{0.0, 0.0}, Snd_new_short, Hp}
		    end;
		%%Ver%%
		ver ->
		    io:format("~n Fall: 6~n"),
		    %% your new pos will be Short() and you will lose your horizontal vel,
		    %% check from Short() to new pos with your remaining ver. vel
		    %%{X_short, Y_short} = Short,
		    {New_x_vel, New_y_vel} = {0.0, Y_vel - (New_short_y - Y_1)},
		    A = {Snd_x_1, Snd_y_1} = {New_short_x, New_short_y},  
		    B = {Snd_x_2, Snd_y_2} = {New_short_x + ?PLAYERWIDTH, New_short_y},
		    C = {Snd_x_3, Snd_y_3} = {New_short_x, New_short_y + ?PLAYERHEIGHT},
		    D = {Snd_x_4, Snd_y_4} = {New_short_x + ?PLAYERWIDTH, New_short_y + ?PLAYERHEIGHT},
		    Snd_ideal_point_1 = {Snd_x_1, Snd_y_1 + New_y_vel},
		    Snd_ideal_point_2 = {Snd_x_2, Snd_y_2 + New_y_vel},
		    Snd_ideal_point_3 = {Snd_x_3, Snd_y_3 + New_y_vel},
		    Snd_ideal_point_4 = {Snd_x_4, Snd_y_4 + New_y_vel},
		    Snd_line_1 = make_line({Snd_x_1, Snd_y_1}, Snd_ideal_point_1),		    
		    Snd_line_2 = make_line({Snd_x_2, Snd_y_2}, Snd_ideal_point_2),		    
		    Snd_line_3 = make_line({Snd_x_3, Snd_y_3}, Snd_ideal_point_3),
		    Snd_line_4 = make_line({Snd_x_4, Snd_y_4}, Snd_ideal_point_4),
		    {_Border_1, Snd_point_1, _Type_1} = border_hit(Snd_line_1, [], Horizontal_list, Dummy_value_v, Dummy_value_h),
		    {_Border_2, Snd_point_2, _Type_2} = border_hit(Snd_line_2, [], Horizontal_list, Dummy_value_v, Dummy_value_h),
		    {_Border_3, Snd_point_3, _Type_3} = border_hit(Snd_line_3, [], Horizontal_list, Dummy_value_v, Dummy_value_h),
		    {_Border_4, Snd_point_4, _Type_4} = border_hit(Snd_line_4, [], Horizontal_list, Dummy_value_v, Dummy_value_h),
		    Snd_short_1 = shortest_distance({Snd_x_1, Snd_y_1}, Snd_ideal_point_1, Snd_point_1),
		    Snd_short_2 = shortest_distance({Snd_x_2, Snd_y_2}, Snd_ideal_point_2, Snd_point_2),
		    Snd_short_3 = shortest_distance({Snd_x_3, Snd_y_3}, Snd_ideal_point_3, Snd_point_3),
		    Snd_short_4 = shortest_distance({Snd_x_4, Snd_y_4}, Snd_ideal_point_4, Snd_point_4),
		    %% io:format("~nPos1: ~w~nPos2: ~w~nPos3: ~w~nPos4: ~w~n", [A,B,C,D]),
		    %% io:format("~nShort1: ~w~nShort2: ~w~nShort3: ~w~nShort4: ~w~nShortest: ~w~n", 
		    %%  	      [Snd_short_1,Snd_short_2,Snd_short_3,Snd_short_4, collision([{Snd_x_1, Snd_y_1},{Snd_x_2, Snd_y_2},
		    %%  									   {Snd_x_3, Snd_y_3},{Snd_x_4, Snd_y_4}],
		    %%  									  [Snd_short_1,Snd_short_2,Snd_short_3,Snd_short_4],
		    %%  									  [Snd_ideal_point_1,Snd_ideal_point_2,
		    %%  									   Snd_ideal_point_3,Snd_ideal_point_4], 
		    %%  									  [Type_1, Type_2, Type_3, Type_4], none)]),
		    case collision([{Snd_x_1, Snd_y_1},{Snd_x_2, Snd_y_2},{Snd_x_3, Snd_y_3},{Snd_x_4, Snd_y_4}],
				   [Snd_short_1,Snd_short_2,Snd_short_3,Snd_short_4],
				   [Snd_ideal_point_1,Snd_ideal_point_2,Snd_ideal_point_3,Snd_ideal_point_4], 
				   [Type_1, Type_2, Type_3, Type_4], none) of
			none ->
			    io:format("~n Fall: 7~n"),
			    %% Bra, ingen krock.
			    %% {Vel, Pos, Hp}
			    {{New_x_vel, New_y_vel}, Snd_ideal_point_1, Hp};
			{Snd_origin, Snd_short, _Type} ->
			    io:format("~n Fall: 8~n"),
			    %% Bra, krock!
			    {Snd_origin_x, Snd_origin_y} = Snd_origin,
			    {Snd_diff_x, Snd_diff_y} = {Snd_x_1 - Snd_origin_x, Snd_y_1 - Snd_origin_y},
			    {Snd_short_x, Snd_short_y} = Snd_short,
			    Snd_new_short = {Snd_short_x + Snd_diff_x, Snd_short_y + Snd_diff_y}, 
			    %% io:format("~new_y_vel: ~w~nnew_short: ~w~n", [New_y_vel,Snd_new_short]),
			    %% io:format("~nkolla här!!~n"),
			    {{0.0, 0.0}, Snd_new_short, Hp}
		    end

	    end
    end.
   

    

iterate_bullet(Server_settings, Player_list, Bullet) ->
    {_Move_factor,			
     Gravity_factor,
     Air_friction,
     _Base_jump_factor,
     _Grid_limit,
     Vel_limit,
     Level_list} = Server_settings,
    {Entity_id, Type, Direction} = Bullet,
    {Player, Rest_list} = get_player(Player_list, Entity_id, []),
    {Name, {X,Y}, Vel, Hp, Power, Id} = Player,
    {X_vel, Y_vel} = Vel,
    if Power =/= 100 ->
	    %% NOT ENOUGH POWER!!!
	    {[{Name, {X,Y}, {limitor(X_vel, Vel_limit, Air_friction), Y_vel - Gravity_factor}, Hp, Power, Id} | Rest_list], nope};
       true ->
	    %% FIRE!!!
	    {X_m, Y_m} = {X+ ?PLAYERMIDDLEX ,Y+ ?PLAYERMIDDLEY},
	    Line = make_line({X_m, Y_m}, Direction),
	    {Vertical_list, Horizontal_list} = get_borders(Level_list, {[],[]}),
	    Dummy_value_v = {{-99999,{-99999,-99999}},{-99999,-99999}, ver},
	    Dummy_value_h = {{{-99999,-99999},-99999},{-99999,-99999}, hor},
	    {_Border_hit, Border_point, _Type}  = border_hit(Line, Vertical_list, Horizontal_list, Dummy_value_v, Dummy_value_h),
	    {V_hit_box_list, H_hit_box_list} = get_hit_boxes(Rest_list, {[],[]}),
	    Hit = player_hit(Line, V_hit_box_list, H_hit_box_list, dummy, dummy),
	    %% TODO %%
	    %% * hit border or player?
	    %% * recoil
	    %% * return the bullet {owner, origin, destination} 
	    if Hit =:= dummy ->
		    %% no player hit, only fire recoil
		    {Fire_player, Rest_list_2} = get_player(Player_list, Entity_id, []),
		    {Name_2, Pos_2, {X_f,Y_f}, Hp_2, _Power_2, Id_2} = Fire_player,
		    io:format("NO HIT!!!"),
		    %% no hit, only fire recoil
		    Border_point,
		    {[{Name_2, Pos_2, {limitor(X_f, Vel_limit, Air_friction), Y_f - Gravity_factor}, Hp_2, 0, Id_2} | Rest_list_2], 
		     {Entity_id, {X_m, Y_m}, Border_point}};
	       true ->
		    io:format("Good! HIT!!!"),
		    %%hit! fire recoil, hit recoil and damage!
		    {Player_id, Point, Damage} = Hit,
		    Short = shortest_distance({X_m, Y_m}, Border_point, Point),
		    if Short =:= Point ->
			    %% player hit! fire recoil and damage!
			    {Hit_player, Rest_list_2} = get_player(Player_list, Player_id, []),
			    {Fire_player, Rest_list_3} = get_player(Rest_list_2, Entity_id, []),
			    {Name_1, Pos_1, Vel_1, Hp_1, Power_1, Id_1} = Hit_player,
			    {Name_2, Pos_2, {X_f,Y_f}, Hp_2, _Power_2, Id_2} = Fire_player,
			    {[{Name_2, Pos_2, {limitor(X_f, Vel_limit, Air_friction),Y_f - Gravity_factor}, Hp_2, 0, Id_2} 
			      | [{Name_1, Pos_1, Vel_1, Hp_1 - Type*Damage, Power_1, Id_1} | Rest_list_3]],{Entity_id, {X_m, Y_m}, Point}};
		       true ->
			    %% wall hit first, only fire recoil
			    {Fire_player, Rest_list_2} = get_player(Player_list, Entity_id, []),
			    {Name_2, Pos_2, {X_f,Y_f}, Hp_2, _Power_2, Id_2} = Fire_player,
			    Border_point,
			    {[{Name_2, Pos_2, {limitor(X_f, Vel_limit, Air_friction) , Y_f - Gravity_factor}, Hp_2, 0, Id_2} | Rest_list_2],
			     {Entity_id, {X_m, Y_m}, Border_point}}
		    end
	    end
    end.
    


get_player([], _Id, _Aux) ->
    io:format("ERROR NO PLAYER FOR BULLET!!!~n"),
    error_no_such_player; %% Error
get_player([{Name, Pos, Vel, Hp, Power, Id} | P_list], E_id, Aux) when E_id =:= Id ->
    {{Name, Pos, Vel, Hp, Power, Id}, lists:append([P_list, Aux])};
get_player([P | P_list], Id, Aux) ->
    get_player(P_list, Id, [P | Aux]).


get_hit_boxes([], Aux) ->
    Aux;
get_hit_boxes([P | P_list], {V_aux, H_aux}) ->
    {_Name, {X, Y}, _Vel, _Hp, _Power, Id} = P,
    Left_head = {X ,{Y+ ?PLAYERHEIGHT - ?PLAYERHEADHEIGHT ,Y+ ?PLAYERHEIGHT}, Id, ?DAMAGEHEAD},
    Right_head = {X+ ?PLAYERWIDTH ,{Y+ ?PLAYERHEIGHT - ?PLAYERHEADHEIGHT ,Y+ ?PLAYERHEIGHT}, Id, ?DAMAGEHEAD},
    Left_body = {X ,{Y+ ?PLAYERLEGSHEIGHT ,Y+ ?PLAYERHEIGHT - ?PLAYERHEADHEIGHT}, Id, ?DAMAGEBODY},
    Right_body = {X+ ?PLAYERWIDTH,{Y+ ?PLAYERLEGSHEIGHT ,Y+ ?PLAYERHEIGHT - ?PLAYERHEADHEIGHT}, Id, ?DAMAGEBODY},
    Left_legs = {X ,{Y ,Y+ ?PLAYERLEGSHEIGHT}, Id, ?DAMAGELEGS},
    Right_legs = {X+ ?PLAYERWIDTH,{Y ,Y+ ?PLAYERLEGSHEIGHT}, Id, ?DAMAGELEGS},
    Bot_body = {{X, X+ ?PLAYERWIDTH}, Y+ ?PLAYERLEGSHEIGHT, Id, ?DAMAGEBODY},
    Top_head = {{X, X+ ?PLAYERWIDTH}, Y+ ?PLAYERHEIGHT, Id, ?DAMAGEHEAD},
    get_hit_boxes(P_list, {[Left_head|[Left_body|[Left_legs|[Right_head|[Right_body|[Right_legs|V_aux]]]]]], [Top_head|[Bot_body|H_aux]]}).

player_hit(Line, [], [], V_close, H_close) ->
    if V_close =:= dummy ->
	    H_close;
       true ->
	    if H_close =:= dummy ->
		    V_close;
	       true ->
		    {{X0,Y0}, _Angle, _Dir} = Line,
		    {_P1_id, Point_1, _Damage_1} = V_close,
		    {_P2_id, Point_2, _Damage_2} = H_close,
		    Short = shortest_distance({X0,Y0}, Point_1, Point_2),
		    if Short =:= Point_1 ->
			    V_close;
		       true ->
			    H_close
		    end
	    end
    end;
player_hit(Line, [], [{{X_start, X_end}, Y, Id, Damage} | H_list], V_close, H_close) ->
    Point = line_hit(Line, {{X_start, X_end}, Y}, hor, 0),
    case Point of 
	nope ->
	    player_hit(Line, [], H_list, V_close, H_close);
	_Point ->
	    if H_close =:= dummy ->
		    player_hit(Line, [], H_list, V_close, {Id, Point, Damage});
	       true ->
		    {{X0,Y0}, _Angle, _Dir} = Line,
		    {_P2_id, Point_2, _Damage_2} = H_close,
		    Short = shortest_distance({X0, Y0}, Point, Point_2),
		    if Short =:= Point ->
			    player_hit(Line, [], H_list, V_close, {Id, Point, Damage});
		       true ->
			    player_hit(Line, [], H_list, V_close, H_close)
		    end
	    end
    end;
player_hit(Line, [{X, {Y_start, Y_end}, Id, Damage} | V_list], H_list, V_close, H_close) ->
    Point = line_hit(Line, {X, {Y_start, Y_end}}, ver, 0),
    case Point of 
	nope ->
	    player_hit(Line, V_list, H_list, V_close, H_close);
	_Point ->
	    if V_close =:= dummy ->
		    player_hit(Line, V_list, H_list, {Id, Point, Damage}, H_close);
	       true ->
		    {{X0,Y0}, _Angle, _Dir} = Line,
		    {_P1_id, Point_1, _Damage_1} = V_close,
		    Short = shortest_distance({X0, Y0}, Point, Point_1),
		    if Short =:= Point ->
			    player_hit(Line, V_list, H_list, {Id, Point, Damage}, H_close);
		       true ->
			    player_hit(Line, V_list, H_list, V_close, H_close)
		    end
	    end
    end.


border_hit(Line, [], [], V_close, H_close) ->
    {{X0,Y0}, _Angle, _Dir} = Line,
    {_V_border, V_point, ver} = V_close,
    {_H_border, H_point, hor} = H_close,
    Short = shortest_distance({X0,Y0}, V_point, H_point),
    if Short =:= V_point ->
            V_close;
       true ->
            H_close
    end;
border_hit(Line, [], [{{X_start, X_end}, Y} | H_list], V_close, H_close) ->
    Point = line_hit(Line, {{X_start, X_end}, Y}, hor, 1),
    case Point of
        nope ->
            border_hit(Line, [], H_list, V_close, H_close);
        _Point ->
            {{X0,Y0}, _Angle, _Dir} = Line,
            {_H_border, H_point, hor} = H_close,
            Short = shortest_distance({X0, Y0}, Point, H_point),
            if Short =:= Point ->
                    border_hit(Line, [], H_list, V_close, {{{X_start,X_end},Y}, Short, hor});
               true ->
                    border_hit(Line, [], H_list, V_close, H_close)
            end
    end;
border_hit(Line, [{X, {Y_start, Y_end}} | V_list], H_list, V_close, H_close) ->
    Point = line_hit(Line, {X, {Y_start,Y_end}}, ver,1),
    case Point of
        nope ->
            border_hit(Line, V_list, H_list, V_close, H_close);
        _Point ->
            {{X0,Y0}, _Angle, _Dir} = Line,
            {_V_border, V_point, ver} = V_close,
            Short = shortest_distance({X0, Y0}, Point, V_point),
            if Short =:= Point ->
                    border_hit(Line, V_list, H_list, {{X,{Y_start,Y_end}}, Point, ver}, H_close);
               true ->
                    border_hit(Line, V_list, H_list, V_close, H_close)
            end
    end.

line_hit(Line, Border, Type, Pad) when Type =:= hor ->
    {{X0,Y0}, Angle, Dir} = Line,
    {{X1,X2}, Y1} = Border,
    case Angle of 
        0 ->
            nope;
	0.0 ->
	    nope;
        _A ->
            if Angle =:= inf ->
                    case Dir of 
                        pos ->
                            if Y0 > Y1 ->
                                    nope;
                               true ->
                                    if X1 < X0 andalso X2 > X0 -> %% greatequl stuf
                                            {X0, Y1-Pad}; %% Fraud
                                       true ->
                                            nope
                                    end
                            end;
                        neg ->
                            if Y0 < Y1 ->
                                    nope;
                               true ->
                                    if X1 < X0 andalso X2 > X0 -> %% greatequl stuf
                                            {X0, Y1+Pad}; %% Fraud
                                       true ->
                                            nope
                                    end 
                            end
                    end;	         
               true ->
		    X = (Y1 - (Y0 - (Angle*X0)))/Angle,
		    case Dir of 
			pos ->
			    if X0 > X ->
				    nope;
			       true ->
				    if X1 < X andalso X2 > X -> %% greatequl stuf
					    if Angle > 0 ->
						    {X, Y1-Pad}; %% Fraud
					       true ->
						    {X, Y1+Pad} %% Fraud
					    end;
				       true ->
					    nope
				    end
			    end;
			neg ->
			    if X0 < X ->
				    nope;
			       true ->
				    if X1 < X andalso X2 > X -> %% greatequl stuf
					    if Angle > 0 ->
						    {X, Y1+Pad}; %% Fraud
					       true ->
						    {X, Y1-Pad} %% Fraud
					    end;
				       true ->
					    nope
				    end
			    end         
		    end	
	    end
    end;
line_hit(Line, Border, Type,Pad) when Type =:= ver ->
    {{X0,Y0}, Angle, Dir} = Line,
    {X1, {Y1, Y2}} = Border,
    case Angle of 
        inf ->
            nope;
        _A ->
            case Dir of
                pos ->
                    if X0 > X1 ->
                            nope;
                       true ->
                            Y = Y0 + (X1 - X0) * Angle,
                            if Y1 < Y andalso Y2 > Y -> %% greatequl stuf
                                    {X1-Pad, Y}; %% Fraud
                               true ->
                                    nope
                            end 
                    end;
                neg ->
                    if X0 < X1 ->
                            nope;
                       true ->
                            Y = Y0 - (X0 - X1) * Angle,
                            if Y1 < Y andalso Y2 > Y -> %% greatequl stuf
                                    {X1+Pad, Y}; %% Fraud
                               true ->
                                    nope
                            end 
                    end       	    	
            end
    end.


%% shorthest_distance(Start_point, Point1, Point2) -> Retunrs the closests point. Point1/Point2.
shortest_distance({Start_x, Start_y}, {X1,Y1}, {X2, Y2}) ->
    Distance1 = math:sqrt( (X1 - Start_x)*(X1 - Start_x) + (Y1 - Start_y)*(Y1 - Start_y)),
    Distance2 = math:sqrt( (X2 - Start_x)*(X2 - Start_x) + (Y2 - Start_y)*(Y2 - Start_y)),
    if Distance1 > Distance2 ->
            {X2, Y2};
       true ->
            {X1, Y1}
    end.


make_line(Pos, Direction) ->
    {X1, Y1} = Pos,
    {X2, Y2} = Direction,
    if X2 - X1 =:= 0 orelse X2 - X1 =:= 0.0 ->
	    Angle = inf;
       true ->
	    Angle = (Y2-Y1) / (X2-X1)
    end,
    if Y2 >= Y1 andalso X2 >= X1 ->
            Dir = pos;
       Y2 >= Y1 andalso X2 =< X1 ->
            Dir = neg;
       Y2 =< Y1 andalso X2 =< X1 ->
            Dir = neg;
       true ->
            Dir = pos
    end,  
    {Pos, Angle, Dir}.


new_power(Power, Vel)->
    {X, Y} = Vel,
    limitor(round(Power + abs(X) + abs(Y)), 100, 0).
