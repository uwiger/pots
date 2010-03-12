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
-module(control).	% asynch_0 (asynch model, blocking hardware API)

-compile(export_all).

-include("messages.hrl").

-record(s, {state = idle}).

start() ->
    asynch_main:event_loop(?MODULE, #s{}).



offhook(?lim, #s{state = idle} = S) ->
    lim:start_tone(dial),
    {ok, S#s{state = getting_first_digit}};
offhook(?lim, #s{state = {ringing_B_side, PidA}} = S) ->
    lim:stop_ringing(),
    PidA ! {?hc, {connect, self()}},
    {ok, S#s{state = {speech, PidA}}};
offhook(From, S) ->
    io:format("Got unknown message in ~p: ~p~n",
	      [S#s.state, {From, offhook}]),
    {ok, S}.



onhook(?lim, #s{state = getting_first_digit} = S) ->
    lim:stop_tone(),
    {ok, S#s{state = idle}};
onhook(?lim, #s{state = {getting_number, {_Number, _ValidSeqs}}} = S) ->
    {ok, S#s{state = idle}};
onhook(?lim, #s{state = {calling_B, _PidB}} = S) ->
    {ok, S#s{state = idle}};
onhook(?lim, #s{state = {ringing_A_side, PidB}} = S) ->
    PidB ! {?hc, {cancel, self()}},
    lim:stop_tone(),
    {ok, S#s{state = idle}};
onhook(?lim, #s{state = {speech, OtherPid}} = S) ->
    lim:disconnect_from(OtherPid),
    OtherPid ! {?hc, {cancel, self()}},
    {ok, S#s{state = idle}};
onhook(?lim, #s{state = {wait_on_hook, HaveTone}} = S) ->
    case HaveTone of
	true ->
	    lim:stop_tone();
	false ->
	    ok
    end,
    {ok, S#s{state = idle}};
onhook(From, S) ->
    io:format("Got unknown message in ~p: ~p~n",
	      [S#s.state, {From, onhook}]),
    {ok, S}.


digit(?lim, Digit, #s{state = getting_first_digit} = S) ->
    lim:stop_tone(),
    case number:analyse(Digit, number:valid_sequences()) of
	invalid ->
	    f_invalid_number(S);
	valid ->
	    f_valid_number(Digit, S);
	{incomplete, ValidSeqs} ->
	    {ok, S#s{state = {getting_number, {Digit, ValidSeqs}}}}
    end;
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

f_invalid_number(S) ->
    lim:start_tone(fault),
    {ok, S#s{state = {wait_on_hook, true}}}.

f_valid_number(Number, S) ->
    PidB = lim:pid_with_telnr(Number),
    PidB ! {?hc, {request_connection, self()}},
    {ok, S#s{state = {calling_B, PidB}}}.


request_connection(?hc, Pid, #s{state = idle} = S) ->
    Pid ! {?hc, {accept, self()}},
    lim:start_ringing(),
    {ok, S#s{state = {ringing_B_side, Pid}}};
request_connection(?hc, Pid, S) ->
    Pid ! {?hc, {reject, self()}},
    {ok, S}.

accept(?hc, PidB, #s{state = {calling_B, PidB}} = S) ->
    lim:start_tone(ring),
    {ok, S#s{state = {ringing_A_side, PidB}}}.

reject(?hc, PidB, #s{state = {calling_B, PidB}} = S) ->
    lim:start_tone(busy),
    {ok, S#s{state = {wait_on_hook, true}}}.

connect(?hc, PidB, #s{state = {ringing_A_side, PidB}} = S) ->
    lim:stop_tone(),
    lim:connect_to(PidB),
    {ok, S#s{state = {speech, PidB}}};
connect(?hc, From, S) ->
    io:format("Got unknown message in ~p: ~p~n",
	      [S#s.state, {From, connect}]),
    {ok, S}.

cancel(?hc, PidA, #s{state = {ringing_B_side, PidA}} = S) ->
    lim:stop_ringing(),
    {ok, S#s{state = idle}};
cancel(?hc, OtherPid, #s{state = {speech, OtherPid}} = S) ->
    {ok, S#s{state = {wait_on_hook, false}}};
cancel(?hc, From, S) ->
    io:format("Got unknown message in ~p: ~p~n",
	      [S#s.state, {From, cancel}]),
    {ok, S}.
