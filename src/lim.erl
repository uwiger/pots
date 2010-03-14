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
-module(lim).
-export([start/0, start/1, start_tone/1, stop_tone/0,
	 start_ringing/0, stop_ringing/0, connect_to/1, disconnect_from/1,
	 telnr_of_pid/1, pid_with_telnr/1, addr_for_pid/1, pid_with_addr/1]).

-export([set_delay/1]).

%% For the start module.
-export([start_lim/0, start_myself/0]).

-include("messages.hrl").

-record(state, {connections = [], delay = 0}).


start()->
    start(sim).

start(Mode)->
    case whereis(lim) of
        undefined->
            start:it(Mode);
        _->
            io:format("simulator already started~n")
    end.

start_lim() ->
    Id =  spawn_link(lim, start_myself, []),
    register(lim, Id).

start_myself() ->
    io:format('Starting LIM~n',[]),
    lim_driver:subscribe(bsu, driver_bsu),
    process_flag(trap_exit, true),    
    put(table,ets:new(table,[private,set])),
    lim(#state{}).


call(Request) ->
    Ref = make_ref(),
    lim ! {request, Request, Ref, self()},
    receive
	{?lim, Ref, {_ReplyTag, Reply}} ->
	    Reply
    end.


set_delay(Ms) when is_integer(Ms), Ms >= 0 ->
    call({set_delay, Ms}).



start_tone(Tone)->
    call({start_tone, Tone}).


stop_tone()->
    call(stop_tone).


start_ringing()->
    call(start_ringing).


stop_ringing()->
    call(stop_ringing).


connect_to(Pid)->
    call({connect_to, Pid}).


disconnect_from(Pid)->
    call({disconnect_from, Pid}).


telnr_of_pid(Pid)->
    {telnr,N} = call({telnr_of_pid, Pid}),
    N.


pid_with_telnr(TelNr)->
    {pid, P} = call({pid_with_telnr, TelNr}),
    P.

%%% continue rewriting...

pid_with_addr(Addr)->
    {pid, P} = call({pid_with_addr, Addr}),
    P.

addr_for_pid(Pid)->
    {addr, A} = call({addr_for_pid, Pid}),
    A.


lim(#state{} = S) ->
    receive
        {Pid, {dts_init,Address}}->
            dts_init(Address),
            ets:insert(get(table),{Address,telnr_init(Address),Pid}),
            link(Pid),
            lim(S);
	{request, Request, Ref, Pid} when is_reference(Ref), is_pid(Pid) ->
	    S1 = handle_call(Request, Ref, Pid, S),
	    lim(S1);
        {lim_driver, {Addr,{key_pressed,{digit,N}}}}->
            addr2pid(Addr) ! {?lim,{digit,N}},
            lim(S);
        {lim_driver, {Addr, on_hook}}->
            addr2pid(Addr) ! {?lim, onhook},
            lim(S);
        {lim_driver, {Addr, off_hook}}->
            addr2pid(Addr) ! {?lim, offhook},
            lim(S);
        {lim_driver, _Other}->
            lim(S);
        {'EXIT',_,system_exit} ->
            io:format('Switch terminating~n', []),
            exit(normal);
        {'EXIT', From, Reason} ->
            io:format('Deleting connections for crashed process ~w~n',
                      [Reason]),
            lim(delete_process(From, S));
        Other ->
            io:format('switch received ~w~n', [Other]),
            lim(S)
    end.

handle_call(Request, Ref, From, #state{connections = Connections,
				       delay = Delay} = S) ->
    case Request of
        {set_delay, Ms} when is_integer(Ms), Ms >= 0 ->
	    send_reply(From, {set_delay_reply, Delay}, Ref, 0),
            S#state{delay = Ms};
        {connect_to,B} ->
            {Res,NewConns}=
		connect(From, pid2addr(From), pid2addr(B), Connections),
            send_reply(From, {connect_reply,Res}, Ref, Delay),
            S#state{connections = NewConns};
        {disconnect_from,B} ->
            {Res,NewConns}=
		disconnect(pid2addr(From),pid2addr(B),Connections),
            send_reply(From, {disconnect_reply,Res}, Ref, Delay),
            S#state{connections = NewConns};
        {telnr_of_pid,OtherPid} ->
            send_reply(From, {telnr,pid2telnr(OtherPid)}, Ref, Delay),
	    S;
        {pid_with_telnr,TelNr} ->
            send_reply(From, {pid_with_telnr_reply,
			      {pid,telnr2pid(TelNr)}}, Ref, Delay),
	    S;
        {addr_for_pid,OtherPid} ->
            send_reply(From, {addr_for_pid_reply,
			      {telnr,pid2addr(OtherPid)}}, Ref, Delay),
	    S;
        {pid_with_addr,Addr} ->
            send_reply(From, {pid_with_addr_reply,
			      {pid,addr2pid(Addr)}}, Ref, Delay),
	    S;
        start_ringing ->
            send_reply(From, {start_ringing_reply,
			     lim_driver:send_cmd(
			       pid2addr(From),
			       {start_ring,intern,normal})},
                       Ref, Delay),
	    S;
        stop_ringing ->
            send_reply(From, {stop_ringing_reply,
			     lim_driver:send_cmd(
			       pid2addr(From),stop_ring)},
		       Ref, Delay),
	    S;
        {start_tone,Tone} ->
            Addr=pid2addr(From),
            ToneAddr = tone:get_tone_sender(Addr),
            tone:start_tone(Addr,Tone,ToneAddr),
            {Res,NewConnections}=connect(From,Addr,ToneAddr,Connections),
            send_reply(From, {start_tone_reply,{Tone,Res}}, Ref, Delay),
	    S#state{connections = NewConnections};
        stop_tone ->
            Addr=pid2addr(From),
            ToneAddr = tone:get_tone_sender(Addr),
            tone:stop_tone(Addr,ToneAddr),
            {Res,NewConnections}=disconnect(Addr,ToneAddr,Connections),
            send_reply(From, {stop_tone_reply,Res}, Ref, Delay),
	    S#state{connections = NewConnections}
    end.




send_reply(Pid, Message, Ref, Delay) ->
    erlang:send_after(Delay, Pid, {?lim, Ref, Message}).

connect(Pid,A,B,Connections)->
    case other_connections(A,B, Connections) of
        {yes, C} ->
            case addr2pid(C) of
                no->
                    {{no,tone_sender},Connections};
                OtherPid->
                    {{no,OtherPid},Connections}
            end;
        no ->
            lim_driver:send_cmd(bsu, {connect,A,1,B,1}),
            {yes, [{A,B,Pid} |Connections]}
    end.

disconnect(A,B,Connections)->
    case delete_connection(A,B, Connections) of
        {no, Connections1}->
            {no, Connections1};
        {{no, C}, Connections1} ->
            case addr2pid(C) of
                no->
                    {{no,tone_sender},Connections1};
                Pid->
                    {{no,Pid},Connections1}
            end;
        {yes, Connections1} ->
            lim_driver:send_cmd(bsu, {disconnect, A,0,B,0}),
            {yes, Connections1}
    end.

other_connections(_,_,[]) ->
    no;
other_connections(A,_, [{A,B,_} | _ ]) ->
    {yes,B};
other_connections(A,_, [{B,A,_} | _ ]) ->
    {yes,B};
other_connections(_,A, [{A,B,_} | _ ]) ->
    {yes,B};
other_connections(_,A, [{B,A,_} | _ ]) ->
    {yes,B};
other_connections(A,B, [_ | T]) ->
    other_connections(A,B, T).

delete_connection(_A, _B, []) ->
    {no, []};
delete_connection(A, B, [{A,C,Pid} | T]) ->
    case B of
        C->
            {yes, T};
        _->
            {{no, C}, [{A,C,Pid} | T]}
    end;
delete_connection(A,B, [{B,C,Pid} | T]) ->
    case A of
        C->
            {yes, T};
        _->
            {{no, C}, [{B,C,Pid} | T]}
    end;
delete_connection(A,B, [H | T]) ->
    {Result, Rest} = delete_connection(A,B, T),
    {Result, [H | Rest]}.


delete_process(Pid, #state{connections = Connections} = S) ->
    NewConns = 
	lists:filter(fun({A,B,PidX}) when Pid == PidX ->
			     lim_driver:send_cmd(bsu, {disconnect, A,0,B,0}),
			     ets:delete(get(table),pid2addr(PidX)),
			     io:format('deleted connection for ~w ~w~n',
				       [A,B]),
			     false;
			(_) ->
			     true
		     end, Connections),
    S#state{connections = NewConns}.

pid2telnr(Pid)->
    filter(ets:match(get(table),{'_','$1',Pid})).

telnr2pid(TelNr)->
    filter(ets:match(get(table),{'_',TelNr,'$1'})).

addr2pid(Addr)->
    filter(ets:match(get(table),{Addr,'_','$1'})).

pid2addr(Pid)->
    filter(ets:match(get(table),{'$1','_',Pid})).

filter([])->no;
filter([[Res]])->Res.


%% Just to get the telnr from subscribers into the table.
%% After that, pick all info about telnrs, Addr:s and pids from table.

telnr_init(Addr) when is_integer(Addr)->
    telnr_to_integer(telnr(Addr,configure:subscribers())).

telnr(Addr,[{TelNr,Addr}|_Subscribers])->
    TelNr;
telnr(Addr,[_|Subscribers])->
    telnr(Addr,Subscribers);
telnr(_Addr,[])->
    %% io:format("WARNING can't find telnr for ~w~n",[Addr]),
    [0].

telnr_to_integer(TelNr)->
    telnr_to_integer(lists:reverse(TelNr),1).

telnr_to_integer([D|Ds],M)->
    D*M+telnr_to_integer(Ds,M*10);
telnr_to_integer([],_)->0.


dts_init(Address) ->
    lim_driver:send_cmd(Address,{activate_dts,
                                 {intern_ring, 1000,5000,1000,5000},
                                 {extern_ring, 500,50,500,4950},
                                 {callback_ring, 200,50,200,50},
                                 {pulse_flash, 100,100,100,500},
                                 {slow_flash,400,400},
                                 {fast_flash,200,200}}),
    lim_driver:send_cmd(Address,clear_display),
    lim_driver:send_cmd(Address,{clear_indicator,all}),
    lim_driver:send_cmd(Address,stop_ring),
    lim_driver:send_cmd(Address,{set_transmission,low_speaking}).

