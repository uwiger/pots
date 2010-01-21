-module(start).

-export([it/1, internal/1]).

it(Type) -> spawn(start, internal, [Type]).

internal(Type) ->
    process_flag(trap_exit, true),
%    case whereis(pxw_server) of
%	undefined ->
%	    pxw:start();
%	_ ->
%	    true
%    end,
    number:start(),
    number_store(subscribers()),
    simulator:start(Type,subscribers()),
    proc_reg:start(),
    lim_driver:start(Type),
%%     switch:start_switch(),
    lim:start_lim(),
%%     lim_asynch:start(),
    elu5:start_elu(configure:elu_start_address()),
    tone:start_tsu(160),
    receive
	{'EXIT',_,simulator_finished} ->
	    io:format("System terminating~n", []),
	    exit(system_exit);
	Bar ->
	    io:format('Start got ~w~n', [Bar])
    end.

number_store([]) -> true;
number_store([{Seq,Mult}|T]) ->
    number:store(Seq,Mult),
    number_store(T).

subscribers() ->	
    configure:subscribers().



