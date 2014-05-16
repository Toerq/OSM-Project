-module(action_db).
-export([get_actions/1, do_call/1, init/1, stop/0, available/1, add/4, remove/2]).
-record(action, {player_id, action, varlist}).

%% init(Db_name) ->
%%     mnesia:create_schema([node()]),
%%     mnesia:start(),
%%     mnesia:create_table(Db_name, 
%% 			[{ram_copies,[node()]},
%% 			 {attributes, 
%% 			  record_info(fields, action)}]),
%%     mnesia:stop().

init(Db_name) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(Db_name, 
			[{ram_copies,[node()]},
			 {attributes, 
			  record_info(fields, action)}]),
    mnesia:wait_for_tables([Db_name], 1000).

stop() ->
    mnesia:stop().


add(Db_name, Player_id, Action, Var_list) ->
    fun() ->
	    case mnesia:read({Db_name, Player_id}) of
		[] ->
		    %% no action, add it
		    Entry = #action{player_id = Player_id, 
				    action = Action, 
				    varlist = Var_list},
		    mnesia:write(Entry),
                    ok;
		[E] ->
                    %% update action
                    E1 = E#action{action = Action, 
				  varlist = Var_list},
                    mnesia:write(E1),
                    ok
	    end
    end.

remove(Db_name, Player_id) ->
    fun() ->
	    case mnesia:read({Db_name, Player_id}) of
		[] ->
		    %% player id not found
		    {error, no_such_player};
		[_E] ->
                    mnesia:delete({Db_name, Player_id})
            end
    end.


available(Db_name) ->
    fun() ->
            mnesia:foldl(fun(X,XS)-> [X|XS]
                        end, 
                        [], 
                        Db_name)
    end.


do_call(C) ->
    Fun = the_func(C),
    mnesia:transaction(Fun).

the_func({action_add, Db_name, Player_id, Action, Var_list})  ->  
    ?MODULE:add(Db_name, Player_id, Action, Var_list);
the_func({action_remove, Db_name, Player_id})  ->  
    ?MODULE:remove(Db_name, Player_id);
the_func({action_available, Db_name})  ->  
    ?MODULE:available(Db_name).

get_actions(Db_name) ->
    {_, Action_list} = ?MODULE:do_call({action_available, Db_name}),
    [{X,Y,Z} ||{_, X, Y, Z} <- Action_list].




%% Pid = spawn(fun() -> test:state_sender() end).
%% game_state:start(action, Pid, 1).
%% Action = {action_add, action, server, add_player, [{"Player1",{0,0},{10, 20}, 100, 1}]}.
%% Action2 = {action_add, action, server, add_player, [{"Player2",{50,50},{20, 20}, 100, 2}]}.
%% Action3 = {action_add, action, server, add_player, [{"Player3",{100,100},{-20, -20}, 100, 3}]}.
%% Action4 = {action_add, action, server, add_player, [{"Player4",{100,100},{-20, 10}, 100, 4}]}.
%% Action5 = {action_add, action, 1, move_up, []}.
%% Action6 = {action_add, action, 2, move_down, []}.
%% Action7 = {action_add, action, 3, move_left, []}.
%% Action8 = {action_add, action, 4, move_right, []}.
%% game_state:register_action(Action).
