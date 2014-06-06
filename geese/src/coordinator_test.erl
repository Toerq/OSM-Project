%% @private
-module(coordinator_test).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


start() ->
   %%geese_sup:start_link(3010).
    geese_coordinator_backup:start_link(),
    geese_coordinator:start_link().


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

coordinator_add_table_test_() ->
    add_table(namn, speltyp, 3),
    [Table|_] = geese_coordinator:browse_tables(),
    {_, Name, Game_type, Connected_players, Max_players} = Table,
    [?_assert({Name, Game_type, Connected_players, Max_players} =:= {namn, speltyp, 0, 3})].

coordinator_add_player_to_table_test_() ->
    add_player(0),
    [Table|_] = geese_coordinator:browse_tables(),
    {_, Name, Game_type, Connected_players, Max_players} = Table,
    [?_assert({Name, Game_type, Connected_players, Max_players} =:= {namn, speltyp, 1, 3})].

coordinator_max_players_in_table_limit_test_() ->
    add_player(0),
    add_player(0),
    add_player(0),
    [Table|_] = geese_coordinator:browse_tables(),
    {_, Name, Game_type, Connected_players, Max_players} = Table,
    [?_assert({Name, Game_type, Connected_players, Max_players} =:= {namn, speltyp, 3, 3})].


coordinator_remove_table_test_() ->
    [Table|_Rest] = table_pids(),
    {_, Table_pid, _} = Table,
    geese_coordinator:remove_table(Table_pid),
    New_tables = table_pids(),
    Boolean = lists:keyfind(Table_pid, 2, New_tables),
    [?_assert(Boolean =:= false)].
    
coordinator_remove_player_test_() ->
    add_table(namn, speltyp, 3),
    add_player(0, reference),
%%    geese_coordinator:remove_player_from_table(),
    geese_coordinator:remove_player_from_table(reference),
    Tables = table_pids(),
    {_, _, {Connected_players, Max_players}} = lists:keyfind(0, 1, Tables),
    [?_assert({Connected_players, Max_players} =:= {0, 3})].
    
    
%%coordinator_crash_test_() ->
%%    add_player(0),
%%    State = geese_coordinator:get_state(),
%%    geese_coordinator:stop(),
%% %%   timer:sleep(10),
%%    Recovered_state = geese_coordinator:get_state(),
%%    [?_assert(Recovered_state =:= State)].
%%coordinator_add_er_test_() ->
%%    add
%%    
