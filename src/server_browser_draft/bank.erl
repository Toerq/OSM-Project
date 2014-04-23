-module(bank).

-export([deposit/2, withdraw/2, balance/1]).

-include("bank.hrl").

add(Who, X) ->
    fun() ->
	    case mnesia:read({server, Who}) of
		[] ->
		    %% no server, add it
		    Entry = #server{server_name=Who,ip=X},
		    mnesia:write(Entry),
		    X;
		[E] ->
                    %% server name is taken, try again
                    {error, server_name_taken}
		    %% Old = E#account.balance,
		    %% New = Old + X,
		    %% E1 = E#account{balance=New},
		    %% mnesia:write(E1),
		    %% New
	    end
    end.

remove(Who, X) ->
    fun() ->
	    case mnesia:read({server, Who}) of
		[] ->
		    %% no server
		    {error, no_such_server};
		[E] ->
                    mnesia:delete(E),
		    %%Old = E#account.balance,
		    %%if 
                    %%Old >= X ->
                    %%   New = Old - X,
                    %%  E1 = E#account{balance=New},
                    %% mnesia:delete(E),
                    %%Old < X ->
                    %%   {error, not_enough_money}
            end
    end.


avail(Who) ->
    fun() ->
	    case mnesia:read({account, Who}) of
		[] ->
		    %% no account
		    {error, no_such_account};
		[E] ->
		    B = E#account.balance,
		    {ok, B}
	    end
    end.
