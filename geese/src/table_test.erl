%% @private
-module(table_test).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-define(NMBR_OF_PLAYERS, 2).



start() ->
   %%geese_sup:start_link(3010).
    geese_table:start_link(?NMBR_OF_PLAYERS, debug).



add_table(Name, Game_type, Max_players) ->
    geese_coordinator:add_table(Name, Game_type, Max_players).


table_pids() ->
    Tables = geese_coordinator:browse_tables(),
    table_pids(Tables, [], 0).
table_pids([], Pid_list, _) ->
    Pid_list;
table_pids([Table|Tables], Pid_list, Index) ->
    {Table_pid, _, _, _, _} = Table,
    {Number_of_players, Max_players, _} = gen_server:call(Table_pid, available_slots),
    Info =   {Number_of_players, Max_players},
    table_pids(Tables, [{Index, Table_pid, Info} | Pid_list], Index + 1).

player_pids() ->
    Players = geese_coordinator:browse_players(),
    player_pids(Players, []).
player_pids([], Pid_list) ->
    Pid_list;
player_pids([Player|Players], Pid_list) ->
    {Pid, _Name, _Socket, _Table_info} = Player,
    player_pids(Players, [Pid | Pid_list]).

add_player(Table_index) ->
    Ref = make_ref(),
    add_player(Table_index, Ref).

add_player(Table_index, Ref) ->
    Tables = table_pids(),
%    {_, _, Table_pid}
    {_, Table_pid, _}  = lists:keyfind(Table_index, 1, Tables),
    
    geese_coordinator:join_lobby(Ref, namn, socket),
    geese_coordinator:join_table(Ref, Table_pid).

init_test_() ->
    start(),
    [?_assert(1 =:= 1)].

init_table_test_() ->
    {Players, Number_of_players, Max_players}  = geese_table:get_state(),
    [?_assert({Number_of_players, Max_players, Players} =:= {0, ?NMBR_OF_PLAYERS, []})].
    
join_table_test_() ->
    Return_tuple = geese_table:join_table(pid01, namn, socket),
    Second_return_tuple = geese_table:join_table(pid01, namn, socket),
    [?_assert(Return_tuple =/= join_failed),
    ?_assert(Second_return_tuple =:= join_failed)].

add_player_test_() ->
    {_, Number_of_players_before, _}  = geese_table:get_state(),
    geese_table:join_table(pid02, namn, socket),
    {Players, Number_of_players_after, _}  = geese_table:get_state(),
    Return = lists:keyfind(pid02, 1, Players),
    Add_player_twice_result = geese_table:join_table(pid02, namn, socket),
    [?_assert(Return =/= false),
     ?_assert(Number_of_players_before < Number_of_players_after),
     ?_assert(Add_player_twice_result =:= join_failed)].


add_player_limit_test_() -> 
    Result = geese_table:join_table(pid03, namn, socket),
    [?_assert(Result =:= join_failed)].

remove_player_test_() ->
    geese_table:remove_player(pid02),
    Remove_non_existing_player = geese_table:remove_player(pid03),
    {Players, Number_of_players, _}  = geese_table:get_state(),
    Result = lists:keyfind(pid03, 1, Players),
    [?_assert(Result =:= false),
     ?_assert(Number_of_players =:= 1),
     ?_assert(Remove_non_existing_player =:= no_such_player_exists)].
    
    
    
