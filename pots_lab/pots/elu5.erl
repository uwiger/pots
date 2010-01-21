%% Process which starts up and supervises the DTS (digital telephone sets)
%% control processes connected to a ELU5 circuit boards. Disgned for
%% Erlang courses.  It contains  hard coded data for which would normally 
%% be stored in a configuration data base.
%%
%% Irresponsible author - Mike Williams




-module(elu5).

-export([start/1, start_elu/1]).

start_elu(Card_Address) ->
	spawn_link(elu5, start, [Card_Address]).


start(Card_Address) ->
	process_flag(trap_exit, true),
	lim_driver:subscribe(Card_Address, driver_elu5),
	lim_driver:send_cmd(Card_Address, {activate_elu5, 8}),
	receive
	     {lim_driver, {Card_Address, {identity, elu5, active, _}}} ->
	         lim_driver:unsubscribe(Card_Address);
	     {lim_driver, {Card_Address, _}} ->
	          exit(did_not_start_elu5_1)
	     after 10000 ->	     % should have started in 10 second
		exit(did_not_start_elu5_2)
	end,
	io:format('ELU at ~w started~n', [Card_Address]),
	start_dts(Card_Address, 7, []).

start_dts(Card_Address, -1, List) ->
    supervise(Card_Address, List);

start_dts(Card_Address, N, List) ->
    Id = spawn_link(dts_lib, dts_start, [Card_Address + N]),
    start_dts(Card_Address, N - 1, [{Id, N}| List]).

	
supervise(Card_Address, List) ->
    receive 
	{'EXIT',_,system_exit} ->
	    io:format("Elu5 terminating~n", []),
	    exit(system_exit);
	{'EXIT', From, Reason} ->
	    case find_and_delete(From, List) of
		{not_found, List1} ->
		    supervise(Card_Address, List1);
		{N, List1} ->
		    Id = spawn_link(dts_lib, dts_start, [Card_Address + N]),
		    io:format('ELU - process ~w restarted ~n', 
			      [Card_Address + N]),
		    supervise(Card_Address, [{Id,N} | List1])
	    end;
	Other ->
	    io:format('Elu5 at ~w got ~w~n', [Card_Address, Other])
    end.

find_and_delete(Pid, []) ->
    {not_found, []};

find_and_delete(Pid, [{Pid,N} | T]) ->
    {N, T};

find_and_delete(Pid, [H | T]) ->
    {Result, Rest}  = find_and_delete(Pid, T),
    {Result, [H | Rest]}.
