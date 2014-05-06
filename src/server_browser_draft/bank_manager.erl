-module(bank_manager).

-export([init_bank/0]).

-include("bank.hrl").

init_bank() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(server, 
			[{disc_copies,[node()]},
			 {attributes, 
			  record_info(fields, server)}]),
    mnesia:stop().
