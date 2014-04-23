-module(bank).

-export([deposit/2, withdraw/2, balance/1]).

-include("bank.hrl").

add(Sname, Ip) ->
    fun() ->
	    case mnesia:read({server, Sname}) of
		[] ->
		    %% no server, add it
		    Entry = #server{server_name = Sname,ip = Ip},
		    mnesia:write(Entry),
		    Ip;
		[E] ->
                    %% server name is taken, try again
                    {error, server_name_taken}
	    end
    end.

remove(Sname, Ip) ->
    fun() ->
	    case mnesia:read({server, Sname}) of
		[] ->
		    %% no server
		    {error, no_such_server};
		[E] ->
                    mnesia:delete({server, Sname}),
            end
    end.


available() ->
    fun() ->
            mnesia:fodl(fun(X,XS)-> [X|XS]
                        end, 
                        [], 
                        server)
    end.

ping(Sname) ->
    fun() ->
            pong
    end.


