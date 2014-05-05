-module(robust_bank_manager).

%% -export([create_schema/0, create_table/0, db_nodes/0]).

%% -include("bank.hrl").
%% -include("reply.hrl").

%% db_nodes() ->
%%     ['one@ubuntu-PKD', 'two@ubuntu-PKD'].

%% create_schema() ->
%%     mnesia:create_schema(db_nodes()).

%% create_table() ->
%%     mnesia:create_table(account, 
%% 			[{disc_copies,db_nodes()},
%% 			 {attributes, 
%% 			  record_info(fields, account)}]),
%%     mnesia:create_table(reply,
%% 			[{disc_copies,db_nodes()},
%% 			 {attributes, 
%% 			  record_info(fields, reply)}
%% 			]).
