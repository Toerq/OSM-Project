-module(bank).

-export([add/2, remove/1, available/0, ping/1]).

-include("bank.hrl").

add(Sname, Ip) ->
    fun() ->
	    case mnesia:read({server, Sname}) of
		[] ->
		    %% no server, add it
		    Entry = #server{server_name = Sname,ip = Ip},
		    mnesia:write(Entry),
		    Ip;
		[_E] ->
                    %% server name is taken, try again
                    {error, server_name_taken}
	    end
    end.

remove(Sname) ->
    fun() ->
	    case mnesia:read({server, Sname}) of
		[] ->
		    %% no server
		    {error, no_such_server};
		[_E] ->
                    mnesia:delete({server, Sname})
            end
    end.


available() ->
    fun() ->
            mnesia:foldl(fun(X,XS)-> [X|XS]
                        end, 
                        [], 
                        server)
    end.

ping(_Sname) ->
    fun() ->
            pong
    end.


