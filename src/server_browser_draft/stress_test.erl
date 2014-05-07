-module(stress_test).

-export([start/0, stop/0]).

stop() ->
    List = [[player | W] || W <- lists:seq(0,9)],
    DestIp = {127,0,0,1},
    % Non-working cleanup below
    Remove = [(fun() -> game_client:removePlayer(L, DestIp) end) || L <- List],
    exit(self(), done).

start() ->
    make_ref(),
    %List = [player2, player3, player4, player5, player6, "player7"],
    List = [[player | W] || W <- lists:seq(0,9)],
    DestIp = {127,0,0,1},
    Pid_list = [{spawn_link(fun() -> stupid_player(L,DestIp) end), L} || L <- List].
    %timer:exit_after(10000, stress_test).

%% Simply move around a player
stupid_player(PlayerName, DestIp) ->
    game_client:addPlayer(PlayerName, DestIp),
    move_player(PlayerName, DestIp).
    
    %game_client:removePlayer(PlayerName, DestIp).

move_player(PlayerName, DestIp) ->
    game_client:move(PlayerName, down, 5, DestIp),
    game_client:move(PlayerName, down, 5, DestIp),
    game_client:move(PlayerName, down, 5, DestIp),
    game_client:move(PlayerName, down, 5, DestIp),
    timer:sleep(50),
    game_client:move(PlayerName, right, 5, DestIp),
    game_client:move(PlayerName, right, 5, DestIp),
    timer:sleep(50),
    game_client:move(PlayerName, up, 5, DestIp),
    game_client:move(PlayerName, up, 5, DestIp),
    timer:sleep(50),
    game_client:move(PlayerName, left, 5, DestIp),
    game_client:move(PlayerName, left, 5, DestIp),
    timer:sleep(50),
    move_player(PlayerName, DestIp).
