-module(player).

-export([addPlayer/1]).

addPlayer(PlayerName) ->
    fun() ->
	    case mnesia:read({player, PlayerName}) of
		[] ->
		    %% add player
		    playerSpawner(PlayerName);
		[_E] ->
		    %% name taken
		    {error, player_name_taken}
	    end
    end.

playerSpawner(PlayerName) ->
    PID = spawn(playerReciver),
    NewPlayer = #player{player_name = PlayerName,
			pid = PID,
			x_pos = rand(), 
			y_pos = rand()},
    mnesia:write(NewPlayer),
    PID.

playerReciver()->
    receive
	{action, From, Action} ->
	    doAction(Action, self()),
	    From ! ok,
	    playerReciver();
	{terminate, From} ->
	    %% remove player
	    doAction(remove_player, self()),
	    From ! {ok, terminated};
	E ->
	    From ! {error, invalid_message, E},
	    playerReciver();
    end.

findPlayer(PID)->
    case mnesia:read({player, PID}) of
	[] ->
	    {error, no_player_found};
	E ->
	    E
    end.

doAction(Action, PID)->
    case findPlayer(PID) of
	{error, no_player_found} ->
	    {error, no_player_found};
	E ->
	    case Action of
		move_up ->
		    game_logic:move(E#player.player_name , up, 5);
		move_down ->
		    game_logic:move(E#player.player_name , down, 5);
		move_right ->
		    game_logic:move(E#player.player_name , right, 5);
		move_left ->
		    game_logic:move(E#player.player_name , left, 5);
		E1 ->
		    {error, unknown_action, E1}
	    end
    end.
