-module(a).

-export([start/0]).

start() ->
    {ok, Pid} = b:start_link(state1, name1),
    {ok, Pid2} = b:start_link(state2, name2),

    {Aa, Bb} = gen_server:call(Pid, checkout),
    {Cc, Dd} = gen_server:call(Pid2, checkout),
    
    
    %% Detta funkar också: 
    % {Aa, Bb} = gen_server:call(Pid, checkout),
    % {Cc, Dd} = gen_server:call(Pid2, checkout),
    %%
    
    %% En gen_server-instans kan ses som ett objekt i java om man gillar OO-tänk.
    
    %% Man kan antingen göra call eller cast till en gen_server-instans
    %% cast är asynkrona meddelanden medans call är synkrona,
    %% man får tillbaka en tupel med som reply

    %% minnesregel: tänk att "call" är som ett telefonsamtal: synkront

    %% Det finns även handle_info, som hanteras ifall man skickar ett
    %% meddelande till server på formen "Pid ! message".
    
        
    io:format("the reply: ~p~nthe state: ~p~n", [Aa, Bb]),
    io:format("the reply2: ~p~nthe state2: ~p~n", [Cc, Dd]).
    

%% följ koden, skriv sedan
%% c(a), c(b), a:start(). för att testa.

