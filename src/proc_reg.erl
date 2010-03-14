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
%% This module contains a process which relates process id's with
%% hardware positions in the switch (ports). 
%%
%% Interface:
%%
%%  proc_reg:getp(Port)   Gets the process associated with Port,
%%                        Links to From.
%%  proc_reg:putp(Port)   Associates Port with process From (error if From
%%                        is already associated with another port
%%  proc_reg:erasep(Port) Disassociates From from any port (error if From
%%                        is not associated to a port
%%  From EXIT Reason      Any port associated with From is disassociated.
%%
%% Iresponsible author Mike Williams

-module(proc_reg).

-export([start/0, reg/0, getp/1, putp/1, erasep/1]).

%______________________________________________________________________
%
% Interface routines
%

getp(Port) ->
	proc_reg ! {self(), get, Port},
	receive 
	    {proc_reg, Pid} -> Pid
	end.

putp(Port) ->
	proc_reg ! {self(), put, Port}.

erasep(Port) ->
	proc_reg ! {erase, Port}.

start() ->
	Id = spawn_link(proc_reg, reg, []),
	register(proc_reg, Id).


reg() ->
	process_flag(trap_exit,true),
	reg([]).

reg(P_List) ->
	receive 
	    {From, get, Port} ->
		From ! {proc_reg,get_process(Port, P_List)},
		reg(P_List);
	    {From, put,Port} ->
		case get_port(From, P_List) of
		    undefined ->
			link(From), 
			reg([{Port,From} | P_List]);
		    _Other ->
			exit(From, double_registration),
			reg(P_List)
			end;
	    {From, erase} ->
		case get_port(From, P_List) of
		    undefined ->
			exit(From, undefined_process),
			reg(P_List);
		    _Other ->
			reg(erase_process(From, P_List))
		    end;
	    {'EXIT',_,system_exit} ->
		io:format('Process register terminating~n', []),
	    exit(normal);

	    {'EXIT', From, _Reason} ->
		reg(erase_process(From, P_List));
	    Other ->
		io:format('proc reg got ~w~n', [Other]),
		reg(P_List)
	end.

get_port(_,[]) ->
	undefined;
get_port(Pid, [{Port, Pid}| _T]) ->
	Port;
get_port(Pid, [{_,_} | T]) ->
	get_port(Pid, T).



get_process(_,[]) ->
	undefined;
get_process(Port, [{Port, Pid}| _T]) ->
	Pid;
get_process(Port, [{_,_} | T]) ->
	get_process(Port, T).

erase_process(_,[]) -> 
	[];
erase_process(Process, [{_,Process}| T]) ->
	T;
erase_process(Process, [H|T]) ->
	[H | erase_process(Process, T)].

