%% @author Johan Gille
-module(action_db).
-export([get_actions/1, do_call/1, init/1, stop/0, available/1, add/4, remove/2, makestring/2]).
-record(action, {player_id, action, varlist}).

%% @doc Starts the mnesia database and a table called Db_name in memory(not disk).
%% === Example ===
%% <div class="example">
%% init(table_1).
%% ok.
%% </div>
-spec init(Db_name::atom()) -> ok.

init(Db_name) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(Db_name, [{ram_copies,[node()]}, {record_name, action}, {attributes, record_info(fields, action)}]),
    mnesia:wait_for_tables([Db_name], 1000),
    ok.

%% @doc Stops the mnesia database.
-spec stop() -> ok.

stop() ->
    mnesia:stop().

%% @doc Will add or update an action in the action database Db_name.
%% With var_list and Action for player_id. 
%% === Example ===
%% <div class="example">
%% add(table_1, 3, move_left, []).
%% </div>
-spec add(Db_name::atom(), Player_id::integer(), Action::atom(), Var_list::list()) -> ok.

add(Db_name, Player_id, Action, Var_list) ->
    fun() ->
	    case mnesia:read({Db_name, Player_id}) of
		[] ->
		    %% no action, add it
		    Entry = #action{player_id = Player_id, 
				    action = Action, 
				    varlist = Var_list},
		    mnesia:write(Db_name, Entry, write);
		[E] ->
                    %% update action
                    E1 = E#action{action = Action, 
				  varlist = Var_list},
                    mnesia:write(E1)
	    end
    end.

%% @doc Removes the current action for player_id from table Db_name.
%% Returns ok or error if no such player is present.
%% === Example ===
%% <div class="example">
%% remove(table_2, 1).
%% </div>
-spec remove(Db_name::atom(), Player_id::integer()) -> ok.

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

%% @doc Returns a folded list of all actions in the Db_name table
-spec available(Db_name::atom()) -> Raw_actions_list::list().

available(Db_name) ->
    fun() ->
            mnesia:foldl(fun(X,XS)-> [X|XS]
                        end, 
                        [], 
                        Db_name)
    end.

%% @doc Will do a mnesia transaction of the function argument.
-spec do_call(C::tuple()) -> ok.

do_call(C) ->
    Fun = the_func(C),
    mnesia:transaction(Fun).

%% @doc A set of functions to be used with do_call. Should not be called by a user.
-spec the_func(Arg_tuple::tuple()) -> ok.

the_func({action_add, Db_name, Player_id, Action, Var_list})  ->  
    ?MODULE:add(Db_name, Player_id, Action, Var_list);
the_func({action_remove, Db_name, Player_id})  ->  
    ?MODULE:remove(Db_name, Player_id);
the_func({action_available, Db_name})  ->  
    ?MODULE:available(Db_name).

%% @doc Will return a list of all actions in the Db_name table.
%% === Example ===
%% <div class="example">
%% get_actions(table_3).
%% Action_list.
%% </div>
-spec get_actions(Db_name::atom()) -> Action_list::list().

get_actions(Db_name) ->
    {_, Action_list} = ?MODULE:do_call({action_available, Db_name}),
    [{X,Y,Z} ||{_, X, Y, Z} <- Action_list].

%% @doc Shifts unsigned ascii from Java to signed Erlang ascii. 
-spec makestring(Sting::list(), Aux::list()) -> List::list().

makestring([], Aux) ->
    lists:reverse(Aux);
makestring([X | XS], Aux) ->
    if X < 0 ->
	    makestring(XS, [256 + X | Aux]);
       true ->
	    makestring(XS, [X | Aux])
    end.
