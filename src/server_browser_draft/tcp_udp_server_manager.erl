-module(tcp_udp_server_manager).

-export([init_actions/0]).

-include("actions.hrl").

init_actions() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(actions, 
			[{disc_copies,[node()]},
			 {attributes, 
			  record_info(fields, actions)}]),
    mnesia:stop().
