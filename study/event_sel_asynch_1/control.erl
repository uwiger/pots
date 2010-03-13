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
-module(control).	% event_fifo_asynch_1 (event-based, blocking hw API)

-compile(export_all).

%%% This is a first (rather naiive) attempt at an event-based control
%%% program using the asynchronous HW API. We come across two obvious
%%% challenges that must be addressed -- involving weird code in three
%%% different places. The program will appear to work until we introduce
%%% delays in the simulator...

-include("messages.hrl").
-record(s, {state = idle}).

start() ->
    asynch_main:event_loop(?MODULE, #s{}).



offhook(?lim, #s{state = idle} = S) ->
    Ref = lim_asynch:start_tone(dial),
    {ok, S#s{state = {{await_tone_start,dial}, getting_first_digit}},
     #recv{lim = Ref, _ = false}};
offhook(?lim, #s{state = {ringing_B_side, PidA}} = S) ->
    Ref = lim_asynch:stop_ringing(),
    PidA ! {?hc, {connect, self()}},
    {ok, S#s{state = {await_ringing_stop, {speech, PidA}}},
    #recv{lim = Ref, _ = false}};
offhook(?lim, S) ->
    io:format("Got unknown message in ~p: ~p~n",
	      [S#s.state, {lim, offhook}]),
    {ok, S}.


start_tone_reply(?lim, {Type, yes},
		 #s{state = {{await_tone_start, Type}, NextState}} = S) ->
    {ok, S#s{state = NextState}}.

stop_tone_reply(?lim, _,
		#s{state = {await_tone_stop, NextState}} = S) ->
    %% CHALLENGE: We must remember to check NextState. An alternative would
    %% be to always perform this check on return, but this would increase
    %% the overhead and increase the risk of entering infinite loops.
    case NextState of
	{continue, Cont} when is_function(Cont) ->
	    Cont(S#s{state = NextState});
	_ ->
	    {ok, S#s{state = NextState}}
    end.

start_ringing_reply(?lim, _,
		    #s{state = {await_ringing_start, NextState}} = S) ->
    {ok, S#s{state = NextState}}.

stop_ringing_reply(?lim, _, % strange return value from 'lim'
		   #s{state = {await_ringing_stop, NextState}} = S) ->
    {ok, S#s{state = NextState}}.

pid_with_telnr_reply(?lim, {pid, Pid},
		     #s{state = {await_pid_with_telnr,
				 request_connection}} = S) ->
    Pid ! {?hc, {request_connection, self()}},
    {ok, S#s{state = {calling_B, Pid}}}.

connect_reply(?lim, yes,
	      #s{state = {await_connect, NextState}} = S) ->
    {ok, S#s{state = NextState}}.

disconnect_reply(?lim, _,
		 #s{state = {await_disconnect, NextState}} = S) ->
    {ok, S#s{state = NextState}}.


onhook(?lim, #s{state = getting_first_digit} = S) ->
    Ref = lim_asynch:stop_tone(),
    {ok, S#s{state = {await_tone_stop, idle}},
     #recv{lim = Ref, _ = false}};
onhook(?lim, #s{state = {getting_number, {_Number, _ValidSeqs}}} = S) ->
    {ok, S#s{state = idle}};
onhook(?lim, #s{state = {calling_B, _PidB}} = S) ->
    {ok, S#s{state = idle}};
onhook(?lim, #s{state = {ringing_A_side, PidB}} = S) ->
    PidB ! {?hc, {cancel, self()}},
    Ref = lim_asynch:stop_tone(),
    {ok, S#s{state = {await_tone_stop, idle}},
     #recv{lim = Ref, _ = false}};
onhook(?lim, #s{state = {speech, OtherPid}} = S) ->
    Ref = lim_asynch:disconnect_from(OtherPid),
    OtherPid ! {?hc, {cancel, self()}},
    {ok, S#s{state = {await_disconnect, idle}},
     #recv{lim = Ref, _ = false}};
onhook(?lim, #s{state = {wait_on_hook, HaveTone}} = S) ->
    case HaveTone of
	true ->
	    Ref = lim_asynch:stop_tone(),
	    {ok, S#s{state = {await_tone_stop, idle}},
	     #recv{lim = Ref, _ = false}};
	false ->
	    {ok, S#s{state = idle}}
    end;
onhook(?lim, S) ->
    io:format("Got unknown message in ~p: ~p~n",
	      [S#s.state, {lim, onhook}]),
    {ok, S}.


digit(?lim, Digit, #s{state = getting_first_digit} = S) ->
    %% CHALLENGE: Since stop_tone() is no longer a synchronous 
    %% operation, continuing with number analysis is no longer 
    %% straightforward. We can either continue and somehow log that 
    %% we are waiting for a message, or we enter the state await_tone_stop
    %% and note that we have more processing to do. The former approach
    %% would get us into trouble if an invalid digit is pressed, since 
    %% we then need to start a fault tone. The latter approach seems more
    %% clear and consistent. NOTE: we must remember to also write 
    %% corresponding code in stop_tone_reply().
    Ref = lim_asynch:stop_tone(),
    {ok, S#s{state = {await_tone_stop,
		      {continue, fun(S1) ->
					 f_first_digit(Digit, S1)
				 end}}},
     #recv{lim = Ref, _ = false}};
digit(?lim, _Digit, #s{state = idle} = S) ->
    {ok, S};
digit(?lim, Digit, #s{state = {getting_number, {Number, ValidSeqs}}} = S) ->
    NewNumber = 10 * Number + Digit,
    case number:analyse(Digit, ValidSeqs) of
	invalid ->
	    f_invalid_number(S);
	valid ->
	    f_valid_number(NewNumber, S);
	{incomplete, NewValidSeqs} ->
	    {ok, S#s{state = {getting_number, {NewNumber, NewValidSeqs}}}}
    end;
digit(?lim, _Digit, S) ->
    {ok, S}.

f_first_digit(Digit, S) ->
    case number:analyse(Digit, number:valid_sequences()) of
	invalid ->
	    f_invalid_number(S);
	valid ->
	    f_valid_number(Digit, S);
	{incomplete, ValidSeqs} ->
	    {ok, S#s{state = {getting_number, {Digit, ValidSeqs}}}}
    end.



f_invalid_number(S) ->
    Ref = lim_asynch:start_tone(fault),
    {ok, S#s{state = {{await_tone_start, fault}, {wait_on_hook, true}}},
     #recv{lim = Ref, _ = false}}.

f_valid_number(Number, S) ->
    Ref = lim_asynch:pid_with_telnr(Number),
    {ok, S#s{state = {await_pid_with_telnr, request_connection}},
    #recv{lim = Ref, _ = false}}.


request_connection(?hc, Pid, #s{state = idle} = S) ->
    Pid ! {?hc, {accept, self()}},
    Ref = lim_asynch:start_ringing(),
    {ok, S#s{state = {await_ringing_start, {ringing_B_side, Pid}}},
     #recv{lim = Ref, _ = false}};
request_connection(?hc, Pid, S) ->
    Pid ! {?hc, {reject, self()}},
    {ok, S}.

accept(?hc, PidB, #s{state = {calling_B, PidB}} = S) ->
    Ref = lim_asynch:start_tone(ring),
    {ok, S#s{state = {{await_tone_start, ring}, {ringing_A_side, PidB}}},
     #recv{lim = Ref, _ = false}}.

reject(?hc, PidB, #s{state = {calling_B, PidB}} = S) ->
    Ref = lim_asynch:start_tone(busy),
    {ok, S#s{state = {{await_tone_start, busy}, {wait_on_hook, true}}},
     #recv{lim = Ref, _ = false}}.

connect(?hc, PidB, #s{state = {ringing_A_side, PidB}} = S) ->
    %% CHALLENGE: This is analogous to the challenge in digit()
    Ref = lim_asynch:stop_tone(),
    {ok, S#s{state = {await_tone_stop, 
		      {continue, fun(S1) ->
					 Ref1 = lim_asynch:connect_to(PidB),
					 {ok, S1#s{state = {await_connect,
							    {speech, PidB}}},
					  #recv{lim = Ref1, _ = false}}
				 end}}},
     #recv{lim = Ref, _ = false}};
connect(?hc, PidB, S) ->
    io:format("Got unknown message in ~p: ~p~n",
	      [S#s.state, {PidB, connect}]),
    {ok, S}.

cancel(?hc, PidA, #s{state = {ringing_B_side, PidA}} = S) ->
    Ref = lim_asynch:stop_ringing(),
    {ok, S#s{state = {await_ringing_stop, {wait_on_hook, false}}},
     #recv{lim = Ref, _ = false}};
cancel(?hc, OtherPid, #s{state = {speech, OtherPid}} = S) ->
    {ok, S#s{state = {wait_on_hook, false}}};
cancel(?hc, Pid, S) ->
    io:format("Got unknown message in ~p: ~p~n",
	      [S#s.state, {Pid, cancel}]),
    {ok, S}.
