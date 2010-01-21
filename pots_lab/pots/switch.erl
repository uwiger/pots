%% Written for the Erlang course.
%%
%% Only included the basic switch and does not include the multi
%% party (conference unit). Only allows double directional connections
%% No switch groups
%% 
%% Stolen and rehacked by Patric Pramdal from 
%% Author Mike Williams (who wouldn't like to see it now).

-module(switch).

%% For the user.
-export([start/0, start/1, start_tone/1, stop_tone/0, 
	 start_ringing/0, stop_ringing/0, connect_to/1, disconnect_from/1,
	 telnr_of_pid/1, pid_with_telnr/1, addr_for_pid/1, pid_with_addr/1]).

-export([set_delay/1]).

%% For the start module.
-export([start_switch/0, start_myself/0]).

-include("messages.hrl").
-define(DEFAULT_DELAY, 0).

start()->
    start(sim).

start(Mode)->
    case whereis(switch) of
	undefined->
	    start:it(Mode);
	_->
	    io:format("simulator already started~n")
    end.

start_switch() ->
    Id =  spawn_link(switch, start_myself, []),
    register(switch, Id).

start_myself() ->
    io:format('Starting switch~n',[]),
    lim_driver:subscribe(bsu, driver_bsu),
    process_flag(trap_exit, true),    
    put(table,ets:new(table,[private,set])),
    switch([], ?DEFAULT_DELAY).


set_delay(Ms) when integer(Ms), Ms >= 0 ->
    switch ! {self(), {set_delay, Ms}},
    receive
	{switch, {set_delay_reply, Res}} ->
	    Res
    end.


start_tone(Tone)->
    switch ! {self(),{start_tone,Tone}},
    receive
	{switch,{start_tone_reply,{Tone,Res}}}->Res
    end.

stop_tone()->
    switch ! {self(),stop_tone},
    receive
	{switch,{stop_tone_reply,Res}}->Res
    end.

start_ringing()->
    switch ! {self(),start_ringing},
    receive
	{switch,{start_ringing_reply,Res}}->Res
    end.

stop_ringing()->
    switch ! {self(),stop_ringing},
    receive
	{switch,{stop_ringing_reply,Res}}->Res
    end.

connect_to(Pid)->
    switch ! {self(),{connect_to,Pid}},
    receive
	{switch,{connect_reply,Res}}->Res
    end.


disconnect_from(Pid)->
    switch ! {self(),{disconnect_from,Pid}},
    receive
	{switch,{disconnect_reply,Res}}->Res
    end.


telnr_of_pid(Pid)->
    switch ! {self(),{pid2telnr,Pid}},
    receive
	{switch,{telnr,N}}->N
    end.

pid_with_telnr(TelNr)->
    switch ! {self(),{telnr2pid,TelNr}},
    receive
	{switch,{pid_with_telnr_reply,{pid,P}}}->P
    end.

pid_with_addr(Addr)->
    switch ! {self(),{addr2pid,Addr}},
    receive
	{switch,{pid,P}}->P
    end.

addr_for_pid(Pid)->
    switch ! {self(),{pid2addr,Pid}},
    receive
	{switch,{addr,A}}->A
    end.    


switch(Connections, Delay) ->
    receive
	{Pid, {set_delay, Ms}} when integer(Ms), Ms >= 0 ->
	    Pid ! {switch, {set_delay_reply, Delay}},
	    switch(Connections, Ms);
	{A,{connect_to,B}} ->
	    {Res,NewConns}=connect(A,pid2addr(A),pid2addr(B),Connections),
	    send_reply(A, {switch,{connect_reply,Res}}, Delay),
%%	    A ! {switch,{connect_reply,Res}},
	    switch(NewConns, Delay);
	{A,{disconnect_from,B}} ->
	    {Res,NewConns}=disconnect(pid2addr(A),pid2addr(B),Connections),
	    send_reply(A, {switch,{disconnect_reply,Res}}, Delay),
%%	    A ! {switch,{disconnect_reply,Res}},
	    switch(NewConns, Delay);
	{Pid,{dts_init,Address}}->
	    dts_init(Address),
	    ets:insert(get(table),{Address,telnr_init(Address),Pid}),
	    link(Pid),
	    switch(Connections, Delay);

	{Pid,{pid2telnr,OtherPid}}->
	    send_reply(Pid, {switch,{telnr,pid2telnr(OtherPid)}}, Delay),
%%	    Pid ! {switch,{telnr,pid2telnr(OtherPid)}},
	    switch(Connections, Delay);
	{Pid,{telnr2pid,TelNr}}->
	    send_reply(Pid, {switch,{pid_with_telnr_reply,
				     {pid,telnr2pid(TelNr)}}}, Delay),
%%	    Pid ! {switch,{pid_with_telnr_reply,{pid,telnr2pid(TelNr)}}},
	    switch(Connections, Delay);
	{Pid,{pid2addr,OtherPid}}->
	    send_reply(Pid, {switch,{telnr,pid2addr(OtherPid)}}, Delay),
%%	    Pid ! {switch,{telnr,pid2addr(OtherPid)}},
	    switch(Connections, Delay);
	{Pid,{addr2pid,Addr}}->
	    send_reply(Pid, {switch,{pid,addr2pid(Addr)}}, Delay),
%%	    Pid ! {switch,{pid,addr2pid(Addr)}},
	    switch(Connections, Delay);

	{Pid,start_ringing}->
	    send_reply(Pid, {switch,{start_ringing_reply,
				     lim_driver:send_cmd(
				       pid2addr(Pid),
				       {start_ring,intern,normal})}},
		       Delay),
%% 	    Pid ! {switch,{start_ringing_reply,
%% 			   lim_driver:send_cmd(pid2addr(Pid),
%% 					       {start_ring,intern,normal})}},
	    switch(Connections, Delay);
	{Pid,stop_ringing}->
	    send_reply(Pid, {switch,{stop_ringing_reply,
				     lim_driver:send_cmd(
				       pid2addr(Pid),stop_ring)}},
		       Delay),
%% 	    Pid ! {switch,{stop_ringing_reply,
%% 			   lim_driver:send_cmd(pid2addr(Pid),stop_ring)}},
	    switch(Connections, Delay);

	{Pid,{start_tone,Tone}}->
	    Addr=pid2addr(Pid),
	    ToneAddr = tone:get_tone_sender(Addr),
	    tone:start_tone(Addr,Tone,ToneAddr),
	    {Res,NewConnections}=connect(Pid,Addr,ToneAddr,Connections),
	    send_reply(Pid, {switch,{start_tone_reply,{Tone,Res}}}, Delay),
%%	    Pid ! {switch,{start_tone_reply,{Tone,Res}}},
	    switch(NewConnections, Delay);
	{Pid,stop_tone}->
	    Addr=pid2addr(Pid),
	    ToneAddr = tone:get_tone_sender(Addr),
	    tone:stop_tone(Addr,ToneAddr),
	    {Res,NewConnections}=disconnect(Addr,ToneAddr,Connections),
	    send_reply(Pid, {switch,{stop_tone_reply,Res}}, Delay),
%%	    Pid ! {switch,{stop_tone_reply,Res}},
	    switch(NewConnections, Delay);

	{lim_driver,{Addr,{key_pressed,{digit,N}}}}->
%%	    send_reply(addr2pid(Addr), {?lim,{digit,N}}, Delay),
	    addr2pid(Addr) ! {?lim,{digit,N}},
	    switch(Connections, Delay);
	{lim_driver,{Addr,on_hook}}->
%%	    send_reply(addr2pid(Addr), {?lim,onhook}, Delay),
	    addr2pid(Addr) ! {?lim,onhook},
	    switch(Connections, Delay);
	{lim_driver,{Addr,off_hook}}->
%%	    send_reply(addr2pid(Addr), {lim,offhook}, Delay),
	    addr2pid(Addr) ! {?lim,offhook},
	    switch(Connections, Delay);
	{lim_driver,Other}->
	    switch(Connections, Delay);
  
	{'EXIT',_,system_exit} ->
	    io:format('Switch terminating~n', []),
	    exit(normal);
	{'EXIT', From, Reason} ->
	    io:format('Deleting connections for crashed process ~w~n',
		      [Reason]),
	    switch(delete_process(From, Connections), Delay);
	Other ->
	    io:format('switch received ~w~n', [Other]),
	    switch(Connections, Delay)
    end.

send_reply(Pid, Message, Delay) ->
    erlang:send_after(Delay, Pid, Message).

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

delete_connection(A,B, []) ->
    {no, []};
delete_connection(A,B, [{A,C,Pid} | T]) ->
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


delete_process(Pid, []) ->
    [];
delete_process(Pid, [{A,B,Pid} | T]) ->
    lim_driver:send_cmd(bsu, {disconnect, A,0,B,0}),
    case ets:delete(get(table),pid2addr(Pid)) of
	true->
	    io:format('deleted connection for ~w ~w~n', [A,B]),
	    delete_process(Pid,T);
	_->
	    io:format("ERROR in switch:delete_process~n")
    end;
delete_process(Pid, [H | T]) ->
    [H | delete_process(Pid,T)].

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

telnr_init(Addr) when integer(Addr)->
    telnr_to_integer(telnr(Addr,configure:subscribers())).

telnr(Addr,[{TelNr,Addr}|Subscribers])->
    TelNr;
telnr(Addr,[_|Subscribers])->
    telnr(Addr,Subscribers);
telnr(Addr,[])->
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
