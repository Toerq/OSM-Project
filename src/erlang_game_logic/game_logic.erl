-module(game_logic).
-export([do_actions/2]).

make_new_state() ->
    {{20,500,10,10}, []}.

do_actions(State, []) ->
    State;
do_actions(State, [{Id, Action, Var_list} | T]) when Id =:= server ->
    do_actions(do_server_action(State, {Action, Var_list}), T);
do_actions({Server_settings, Player_list}, [A | T]) ->
    do_actions({Server_settings, do_actionsAux(Server_settings, Player_list, [], A)}, T).

do_server_action({Server_settings, Player_list}, {Action, Var_list}) ->
    case Action of
	add_player ->
	    [Player|_] = Var_list,
	    {Server_settings, add_player(Player, Player_list, [])};
	change_settings ->
	    {change_settings(Server_settings, Var_list), Player_list};
	remove_player ->
	    [Player|_] = Var_list,
	    {Server_settings, remove_player(Player, Player_list, [])}
    end.

do_actions_aux(_Server_settings, [], P_list, _Action)->
    P_list;
do_actions_aux(Server_settings, [{Name, Pos, Vel, Hp, Id} | T], P_list, {Player_id, Action, _Var_list}) when Id =:= Player_id ->
    do_actions_aux(Server_settings, T, [do_action(Server_settings, {Name, Pos, Vel, Hp, Id}, Action) | P_list], {Player_id, Action});
do_actions_aux(Server_settings, [{Name, Pos, Vel, Hp, Id} | T], P_list, {Player_id, Action, _Var_list}) when Player_id =:= global ->
    do_actions_aux(Server_settings, T, [do_action(Server_settings, {Name, Pos, Vel, Hp, Id}, Action) | P_list], {Player_id, Action});
do_actions_aux(Settings, [P | T], P_list, Action) ->
    do_actions_aux(Settings, T, [P | P_list], Action).

do_action({Player_move_factor, Grid_limit, Vel_limit, Friction}, {Name_string, Pos, Vel, Hp, Id}, Action) ->
    case Action of
	move_up ->
	    {{New_x_pos, New_y_pos}, {New_x_vel, New_y_vel}} = pos_change(Pos, Vel, {0,Player_move_factor}, Vel_limit, Grid_limit, Friction),
	    {Name_string, {New_x_pos, New_y_pos}, {New_x_vel, New_y_vel}, Hp, Id};
	move_down ->
	    {{New_x_pos, New_y_pos}, {New_x_vel, New_y_vel}} = pos_change(Pos, Vel, {0,-Player_move_factor}, Vel_limit, Grid_limit, Friction),
	    {Name_string, {New_x_pos, New_y_pos}, {New_x_vel, New_y_vel}, Hp, Id};
	move_left ->
	    {{New_x_pos, New_y_pos}, {New_x_vel, New_y_vel}} = pos_change(Pos, Vel, {-Player_move_factor,0}, Vel_limit, Grid_limit, Friction),
	    {Name_string, {New_x_pos, New_y_pos}, {New_x_vel, New_y_vel}, Hp, Id};
	move_right ->
	    {{New_x_pos, New_y_pos}, {New_x_vel, New_y_vel}} = pos_change(Pos, Vel, {Player_move_factor,0}, Vel_limit, Grid_limit, Friction),
	    {Name_string, {New_x_pos, New_y_pos}, {New_x_vel, New_y_vel}, Hp, Id};
	stop ->
	    {{New_x_pos, New_y_pos}, {New_x_vel, New_y_vel}} = pos_change(Pos, Vel, {0,0}, Vel_limit, Grid_limit, Friction),
	    {Name_string, {New_x_pos, New_y_pos}, {New_x_vel, New_y_vel}, Hp, Id}
    end.

pos_change({X_pos, Y_pos}, {X_vel, Y_vel}, {X_vel_change, Y_vel_change}, Vel_limit, Grid_limit, Friction) ->
    New_x_vel = limitor(X_vel + X_vel_change, Vel_limit, Friction),
    New_y_vel = limitor(Y_vel + Y_vel_change, Vel_limit, Friction),
    New_x_pos = modulor(X_pos + New_x_vel, Grid_limit),
    New_y_pos = modulor(Y_pos + New_y_vel, Grid_limit),
    {{New_x_pos, New_y_pos}, {New_x_vel, New_y_vel}}.

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

add_player(Player, [], Aux_list) ->
    [Player | Aux_list];
add_player(Player, [P | T], Aux_list) when P =/= Player ->
    add_player(Player, T, [P | Aux_list]);
add_player(_ , Players, Aux_list) ->
    lists:append([Players, Aux_list]).

remove_player(_Player, [], Aux_list) ->
    Aux_list;
remove_player(Player, [P | T], Aux_list) when P =:= Player ->
    lists:append([T, Aux_list]);
remove_player(Player, [P | T], Aux_list) ->
    remove_player(Player, T, [P | Aux_list]).


change_settings({Vel_factor, Grid_limit, Vel_limit, Friction}, New_settings) ->
    Old_settings = [Vel_factor, Grid_limit, Vel_limit, Friction],
    [Vel_f, Grid_l, Vel_l, Fric] = settings_update(Old_settings, New_settings, []),
    {Vel_f, Grid_l, Vel_l, Fric}.

settings_update([],[], Aux) ->
    lists:reverse(Aux);
settings_update([O | OT], [N | NT], Aux) when N =:= no_change ->
    settings_update(OT, NT, [O | Aux]);
settings_update([_O | OT], [N | NT], Aux) ->
    settings_update(OT, NT, [N | Aux]).
