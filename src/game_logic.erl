-module(game_logic).

-export([init_game/0]).

init_game() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(player, 
			[{disc_copies,[node()]},
			 {attributes, 
			  record_info(fields, player)}]),
    mnesia:stop().

addPlayer(PlayerName) ->
    fun() ->
	    case mnesia:read({player, PlayerName}) of
		[] ->
		    %% add player
		    NewPlayer = #player{player_name = PlayerName, {x_pos = rand(), y_pos = rand()}},
		    mnesia:write(NewPlayer),
		    ok;
		[_E] ->
		    %% name taken
		    {error, player_name_taken}
	    end
    end.

removePlayer(PlayerName) ->
    fun() ->
	    case mnesia:read({player, PlayerName}) of
		[] ->
		    %% no player
		    {error, no_such_player};
		[_E] ->
                    mnesia:delete({player, PlayerName})
            end
    end.

getPos(PlayerName) ->
    fun() ->
	    case mnesia:read({player, PlayerName}) of
		[] ->
		    %% player not found, add player
		    %%NewPlayer = #player{player_name = PlayerName, {x_pos = rand(), y_pos = rand()}},
		    %%mnesia:write(NewPlayer),
		    %%{NewPlayer#player.x_pos,NewPlayer#player.y_pos};
		    {error, player_not_found};
		[E] ->
		    %% give current pos
		    {E#player.x_pos, E#player.y_pos}
	    end
    end.

getAllPos() ->
    fun() ->
	    mnesia:foldl(fun(X,XS) -> [X|XS] end, [], player)
    end.


move(PlayerName, Direction, Amount) ->
    fun() ->
	    case mnesia:read({player, PlayerName}) of
		[] ->
		    %% player not found
		    {error, not_a_player};
		[E] ->
		    %% move player Amount of units in Diretction
		    case of Direction
			up ->
			    E1 = E#server{y_pos = E#player.y_pos + Amount},	      
			    mnesia:write(E1),
			    ok;
			down ->
			    E1 = E#server{y_pos = E#player.y_pos - Amount},	      
			    mnesia:write(E1),
			    ok;
			left ->
			    E1 = E#server{x_pos = E#player.x_pos - Amount},
			    mnesia:write(E1),
			    ok;
			right ->
			    E1 = E#server{x_pos = E#player.x_pos + Amount},	      
			    mnesia:write(E1),
			    ok;
			_X ->
			    {error, badarg_direction}
		    end
	    end
    end.

