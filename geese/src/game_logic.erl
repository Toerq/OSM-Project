-module(game_logic).
-compile(export_all).
-export([do_actions/2, make_new_state/0]).

-define(PLAYERHEIGHT, 70).
-define(PLAYERWIDTH, 26).
-define(PLAYERLEGSHEIGHT, 24).
-define(PLAYERBODYHEIGHT, 26).
-define(PLAYERHEADHEIGHT, 20).
-define(PLAYERMIDDLEX, 15).
-define(PLAYERMIDDLEY, 40).

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
      [{{0, 0},{800, 600}},{{250,100},{400,110}},{{500,200},{650,210}},{{-10,-10},{810,5}}]}, 
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
	    Dir_vel = -Move_factor;
	stop ->
	    Dir_vel = 0.0
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
	    {Name, Pos, {limitor(X_vel, Vel_limit, Air_friction), Y_vel-Gravity_factor}, Hp, E_id}
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
add_player({Name, Pos, Vel, Hp, Id}, [{N, P, V, H, I} | T], Aux_list) when I =/= Id ->
    add_player({Name, Pos, Vel, Hp, Id}, T, [{N, P, V, H, I} | Aux_list]);
add_player(_ , Players, Aux_list) ->
    lists:append([Players, Aux_list]).

remove_player(_Player, [], Aux_list) ->
    Aux_list;
%%remove_player(Player, [P | T], Aux_list) when P =:= Player ->
remove_player({_Name, _Pos, _Vel, _Hp, Id}, [{_N, _P, _V, _H, I} | T], Aux_list) when I =:= Id ->
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
    iterate_state_aux({Server_settings, {New_player_list, Bullet_list}}, Aux_list, [Bullet_info |Bullet_info_list]).

iterate_player(Server_settings, Player) ->    
    {Name, Pos, Vel, Hp, Id} = Player,
    {_Move_factor,
     _Gravity_factor,
     _Air_friction,
     _Base_jump_factor,
     _Grid_limit,
     _Vel_limit,
     Level_list} = Server_settings,
    {New_vel, New_pos, New_hp} = iterate_move(Vel, Pos, Hp, Level_list), 
    {Name, New_pos, New_vel, New_hp, Id}.

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

iterate_move(Vel, Pos, Hp, Level_list) ->
    {Vertical_list, Horizontal_list} = get_borders(Level_list, {[],[]}),
    {X_vel, Y_vel} = Vel,
    {X, Y} = Pos,
    Ideal_point = {X+X_vel, Y+Y_vel},
    Line = make_line(Pos, Ideal_point),
    Dummy_value_v = {{-99999,{-99999,-99999}},{-99999,-99999}, ver},
    Dummy_value_h = {{{-99999,-99999},-99999},{-99999,-99999}, hor},
    {_Border_hit, Point, Type} = border_hit(Line, Vertical_list, Horizontal_list, Dummy_value_v, Dummy_value_h),
    Short = shortest_distance(Pos, Ideal_point, Point),
    if Short =:= Ideal_point ->
            %% BRA inge krock
	    %% io:format("Fall1: Ingen krock"),
	    {{X_vel, Y_vel},Ideal_point, Hp}; 
       true ->
            %% Krock, stuff
	    case Type of 
		hor ->
		    %% your new pos will be Short() and you will lose your vertical vel,
		    %% check from Short() to new pos with your remaining hor. vel
		    {X_short, Y_short} = Short,
		    {New_x_vel, New_y_vel} = {X_vel - (X_short-X), 0.0},
		    Ideal_point_2 = {X_short + New_x_vel, Y_short},
		    Line_2 = make_line(Short, Ideal_point_2),
		    {_Border_hit_2, Point_2, _Type_2}  = border_hit(Line_2, Vertical_list, [], Dummy_value_v, Dummy_value_h),
		    Short_2 = shortest_distance(Short, Ideal_point_2, Point_2),
		    if Short_2 =:= Ideal_point_2 ->
			    %% BRA ingen krock
			    %% io:format("Fall2: Hor. Krock bara~n~w~n~w~n", [Short, Ideal_point_2]),
			    {{X_vel, 0.0},Ideal_point_2, Hp};
		       true ->
			    %% BRA krock!
			    %% io:format("Fall3: Hor. Krock sen Ver. krock"),
			    {{0.0, 0.0},Short_2, Hp}
		    end;
		ver ->
		    %% your new pos will be Short() and you will lose your hor vel,
		    %% check from Short() to new pos with your remaining ver. vel
		    {X_short, Y_short} = Short,
		    {New_x_vel, New_y_vel} = {0.0, Y_vel - (Y_short-Y)},
		    Ideal_point_2 = {X_short, Y_short+New_y_vel},
		    Line_2 = make_line(Short, {X_short, Y_short+New_y_vel}),
		    {_Border_hit_2, Point_2, _Type_2}  = border_hit(Line_2, [], Horizontal_list, Dummy_value_v, Dummy_value_h),
		    Short_2 = shortest_distance(Short, Ideal_point_2, Point_2),
		    if Short_2 =:= Ideal_point_2 ->
			    %% BRA ingen krock
			    %% io:format("Fall4: Ver. Krock bara~n~w~n~w~n",[Short, Line_2]),
			    {{0.0, Y_vel},Ideal_point_2, Hp};
		       true ->
			    %% BRA krock!
			    %% io:format("Fall5: Verr. Krock sen Hor. krock"),
			    {{0.0, 0.0},Short_2, Hp}
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
    {_Name, {X,Y}, _Vel, _Hp, _Id} = Player,
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
	    {Name_2, Pos_2, {X_f,Y_f}, Hp_2, Id_2} = Fire_player,
	    io:format("NO HIT!!!"),
	    %% no hit, only fire recoil
	    Border_point,
	    {[{Name_2, Pos_2, {limitor(X_f, Vel_limit, Air_friction), Y_f - Gravity_factor}, Hp_2, Id_2} | Rest_list_2], 
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
		    {Name, Pos, Vel, Hp, Id} = Hit_player,
		    {Name_2, Pos_2, {X_f,Y_f}, Hp_2, Id_2} = Fire_player,
		    {[{Name_2, Pos_2, {limitor(X_f, Vel_limit, Air_friction),Y_f - Gravity_factor}, Hp_2, Id_2} 
		      | [{Name, Pos, Vel, Hp - Type*Damage, Id} | Rest_list_3]],{Entity_id, {X_m, Y_m}, Point}};
	       true ->
		    %% wall hit first, only fire recoil
		    {Fire_player, Rest_list_2} = get_player(Player_list, Entity_id, []),
		    {Name_2, Pos_2, {X_f,Y_f}, Hp_2, Id_2} = Fire_player,
		    Border_point,
		    {[{Name_2, Pos_2, {limitor(X_f, Vel_limit, Air_friction) , Y_f - Gravity_factor}, Hp_2, Id_2} | Rest_list_2],
		     {Entity_id, {X_m, Y_m}, Border_point}}
	    end
    end.


get_player([], _Id, _Aux) ->
    io:format("ERROR NO PLAYER FOR BULLET!!!~n"),
    error_no_such_player; %% Error
get_player([{Name, Pos, Vel, Hp, Id} | P_list], E_id, Aux) when E_id =:= Id ->
    {{Name, Pos, Vel, Hp, Id}, lists:append([P_list, Aux])};
get_player([P | P_list], Id, Aux) ->
    get_player(P_list, Id, [P | Aux]).


get_hit_boxes([], Aux) ->
    Aux;
get_hit_boxes([P | P_list], {V_aux, H_aux}) ->
    {_Name, {X, Y}, _Vel, _Hp, Id} = P,
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
    Point = line_hit(Line, {X, {Y_start,Y_end}}, ver, 1),
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
                                    if X1 =< X0 andalso X2 >= X0 ->
                                            {X0, Y1-Pad}; %% Fraud
                                       true ->
                                            nope
                                    end
                            end;
                        neg ->
                            if Y0 < Y1 ->
                                    nope;
                               true ->
                                    if X1 =< X0 andalso X2 >= X0 ->
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
				    if X1 =< X andalso X2 >= X ->
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
				    if X1 =< X andalso X2 >= X ->
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
                            if Y1 =< Y andalso Y2 >= Y ->
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
                            if Y1 =< Y andalso Y2 >= Y ->
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
