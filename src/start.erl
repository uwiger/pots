%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
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



