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
-module(control_lib).
-export([start_tone/1, stop_tone/0, start_ringing/0, stop_ringing/0,
	 connect_to/1, disconnect_from/1, msg/0, send/2,
	 print/1, print/2, print/4]).

start_tone(Tone)->
    case lim:start_tone(Tone) of
	yes->
	    print("started ~w tone",[Tone]),
	    yes;
	{no, tone_sender}->
	    print("ERROR tried to start tone when already started"),
	    {no, tone_sender};
	{no, OtherPid}->
	    print("ERROR tried to start tone when connected to ~w (~w)",
		  [OtherPid,number(OtherPid)]),
	    {no, OtherPid};
	_Other->
	    print("ERROR in control_lib:start_tone/1"),
	    error
    end.

stop_tone()->
    case lim:stop_tone() of
	yes->
	    print("stopped tone"),
	    yes;
	no->
	    print("ERROR tried to stop tone without being started"),
	    no;
	{no, OtherPid} when OtherPid=/=tone_sender->
	    print("ERROR tried to stop tone without being started"),
	    {no, OtherPid};
	_Other->
	    print("ERROR in control_lib:stop_tone/0"),
	    error
    end.

start_ringing()->
    print("started ringing"),
    lim:start_ringing(),
    ok.

stop_ringing()->
    print("stopped ringing"),
    lim:stop_ringing(),
    ok.

connect_to(Pid)->
    case lim:connect_to(Pid) of
	yes->
	    print("connected to ~w (~w)",[Pid,number(Pid)]),
	    yes;
	{no, tone_sender}->
	    print("ERROR connect failed, already connected to tone sender"),
	    {no, tone_sender};
	{no, OtherPid}->
	    print("ERROR connect failed, already connected to ~w (~w)",
		  [OtherPid,number(OtherPid)]),
	    {no, OtherPid};
	_Other->
	    print("ERROR in control_lib:connect_to/1"),
	    error
    end.

disconnect_from(Pid)->
    case lim:disconnect_from(Pid) of
	yes->
	    print("disconnected from ~w (~w)",[Pid,number(Pid)]),
	    yes;
	no->
	    print("ERROR disconnect failed, no connection to ~w (~w)",
		  [Pid,number(Pid)]),
	    no;
	{no, OtherPid}->
	    print("ERROR disconnect failed, no connection to ~w (~w)",
		  [Pid,number(Pid)]),
	    {no, OtherPid};
	_Other->
	    print("ERROR in control_lib:disconnect_from/1"),
	    error
    end.

send(Pid,Msg)->
    Self=self(),
    case Pid of
	Self->
	    print("WARNING tried to send to myself");
	Pid when is_pid(Pid)->
	    Pid ! Msg,
	    print("sent ~w to ~w",[Msg,Pid]);
	_->
	    print("~w is not a pid",[Pid])
    end.

msg()->
    receive
	{lim,Msg}->
	    io:nl(),
	    print("got  ~w",[Msg]),
	    {lim,Msg};
	Msg->
	    print("got  ~w",[Msg]),
	    Msg
    end.

print(String)->
    print(String,[]).

print(String,Args)->
    Self=pid_to_list(self()),
    MyNumber=lists:append(["(",integer_to_list(my_number()),")"]),
    print(Self,MyNumber,String,Args).

print(Id1,Id2,String,Args)->
    print("~-10s~-7s",String,Id1,Id2,Args).

print(String1,String2,Id1,Id2,Args)->
    NewString=lists:append([String1,String2,"~n"]),
    io:format(NewString,[Id1,Id2|Args]).

my_number()->
    number(self()).

number(Pid)->
    lim:telnr_of_pid(Pid).
