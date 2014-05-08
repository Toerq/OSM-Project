-module(server_browser).

-export([start/0, stop/0, init/0]).

-define(PORT, 3010).

-define(CONNECTIONS, 50).

init() ->
    server_browser_fun:init_mnesia().

start() ->
    mnesia:start(),
    spawn_link(fun() -> server(?PORT, ?CONNECTIONS) end).

stop() ->
    mnesia:stop(),
    tcp_server:stop(?PORT).

server(Port, N) ->
    tcp_server:start_raw_server(Port, fun(Socket) -> input_handler(Socket) end, N, 4).

input_handler(Socket) ->
    receive
 	{tcp, Socket, Bin} ->
 	    Term = binary_to_term(Bin),
	    %% io:format("~p", [Term]),
	    Reply = do_call(Term),
 	    send_term(Socket, Reply),
	    input_handler(Socket);
 	{tcp_closed, Socket} ->
	    true
     end.

send_term(Socket, Term) ->
    gen_tcp:send(Socket, [term_to_binary(Term)]).

do_call(C) ->
    Fun = the_func(C),
    mnesia:transaction(Fun).

the_func({add, Server_Name, Ip})  ->  server_browser_fun:add(Server_Name, Ip);
the_func({remove, Server_Name}) ->  server_browser_fun:remove(Server_Name);
the_func({available}) ->  server_browser_fun:available();
the_func({clear}) ->  server_browser_fun:clear();
the_func({terminate}) ->  server_browser:stop().
