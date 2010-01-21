%%% Copyright (C) 1998 Ericsson Software Technology AB, Erlang Systems
%%% File    : control.erl
%%% Author  : Håkan Huss <hakan@erlang.ericsson.se>
%%% Purpose : Proposed solution to exercise 5.B
%%% Created : 12 Jun 1998 by Håkan Huss <hakan@erlang.ericsson.se>

-module(control).
-author('hakan@erlang.ericsson.se').

-export([start/0]).

-include("messages.hrl").

start() ->
    idle().

idle() ->
    receive
	{?lim, offhook} ->
	    lim:start_tone(dial),
	    getting_first_digit();
	{?lim, {digit, _Digit}} ->
	    idle();
	{?hc, {request_connection, Pid}} ->
	    Pid ! {?hc, {accept, self()}},
	    lim:start_ringing(),
	    ringing_B_side(Pid);
	Other ->
	    io:format("Got unknown message in idle: ~p~n", [Other]),
	    idle()
    end.

getting_first_digit() ->
    receive
	{?lim, onhook} ->
	    lim:stop_tone(),
	    idle();
	{?lim, {digit, Digit}} ->
	    lim:stop_tone(),
	    getting_number(Digit,
			   number:analyse(Digit, number:valid_sequences()));
	{?hc, {request_connection, Pid}} ->
	    Pid ! {?hc, {reject, self()}},
	    getting_first_digit();
	Other ->
	    io:format("Got unknown message in getting_first_digit: ~p~n",
		      [Other]),
	    getting_first_digit()
    end.

getting_number(_Number, invalid) ->
    lim:start_tone(fault),
    wait_on_hook(true);
getting_number(Number, valid) ->
    PidB = lim:pid_with_telnr(Number),
    PidB ! {?hc, {request_connection, self()}},
    calling_B(PidB);
getting_number(Number, {incomplete, ValidSeqs}) ->
    receive
	{?lim, onhook} ->
	    idle();
	{?lim, {digit, Digit}} ->
	    getting_number(10 * Number + Digit,
			   number:analyse(Digit, ValidSeqs));
	{?hc, {request_connection, Pid}} ->
	    Pid ! {?hc, {reject, self()}},
	    getting_number(Number, {incomplete, ValidSeqs});
	Other ->
	    io:format("Got unknown message in getting_number: ~p~n", [Other]),
	    getting_number(Number, {incomplete, ValidSeqs})
    end.

calling_B(PidB) ->
    receive
	{?lim, onhook} ->
	    idle();
	{?lim, {digit, _Digit}} ->
	    calling_B(PidB);
	{?hc, {accept, PidB}} ->
	    lim:start_tone(ring),
	    ringing_A_side(PidB);
	{?hc, {reject, PidB}} ->
	    lim:start_tone(busy),
	    wait_on_hook(true);
	{?hc, {request_connection, Pid}} ->
	    Pid ! {?hc, {reject, self()}},
	    calling_B(PidB);
	Other ->
	    io:format("Got unknown message in calling_B(~p): ~p~n",
		      [PidB, Other]),
	    calling_B(PidB)
    end.

ringing_A_side(PidB) ->
    receive
	{?hc, {connect, PidB}} ->
	    lim:stop_tone(),
	    lim:connect_to(PidB),
	    speech(PidB);
	{?lim, onhook} ->
	    PidB ! {?hc, {cancel, self()}},
	    lim:stop_tone(),
	    idle();
	{?lim, {digit, _Digit}} ->
	    ringing_A_side(PidB);
	{?hc, {request_connection, Pid}} ->
	    Pid ! {?hc, {reject, self()}},
	    ringing_A_side(PidB);
	Other ->
	    io:format("Got unknown message in ringing_A_side: ~p~n", [Other]),
	    ringing_A_side(PidB)
    end.

speech(OtherPid) ->
    receive
	{?lim, onhook} ->
	    lim:disconnect_from(OtherPid),
	    OtherPid ! {?hc, {cancel, self()}},
	    idle();
	{?lim, {digit, _Digit}} ->
	    speech(OtherPid);
	{?hc, {cancel, OtherPid}} ->
	    wait_on_hook(false);
	{?hc, {request_connection, Pid}} ->
	    Pid ! {?hc, {reject, self()}},
	    speech(OtherPid);
	Other ->
	    io:format("Got unknown message in speech: ~p~n", [Other]),
	    speech(OtherPid)
    end.

wait_on_hook(Have_tone) ->
    receive
	{?lim, onhook} ->
	    case Have_tone of
		true ->
		    lim:stop_tone();
		_ ->
		    nothing
	    end,
	    idle();
	{?lim, {digit, _Digit}} ->
	    wait_on_hook(Have_tone);
	{?hc, {request_connection, Pid}} ->
	    Pid ! {?hc, {reject, self()}},
	    wait_on_hook(Have_tone);
	Other ->
	    io:format("Got unknown message in wait_on_hook: ~p~n", [Other]),
	    wait_on_hook(Have_tone)
    end.

ringing_B_side(PidA) ->
    receive
	{?lim, offhook} ->
	    lim:stop_ringing(),
	    PidA ! {?hc, {connect, self()}},
	    speech(PidA);
	{?hc, {cancel, PidA}} ->
	    lim:stop_ringing(),
	    idle();
	{?lim, {digit, _Digit}} ->
	    ringing_B_side(PidA);
	{?hc, {request_connection, Pid}} ->
	    Pid ! {?hc, {reject, self()}},
	    ringing_B_side(PidA);
	Other ->
	    io:format("Got unknown message in ringing_B_side: ~p~n", [Other]),
	    ringing_B_side(PidA)
    end.
