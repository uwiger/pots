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
%%  
%% Module for the sending of tones. This should contain an allocator but
%% is dispensed with here for reasons of simplicity. Instead each
%% dts is assigned a tone sender, in this case as follows
%%
%%  tsu port = dts port + Const
%%
%%  This should be changed!!!!


-module(tone).

%% (patric) get_tone_sender
-export([start_tsu/1, tsu/2, start_tone/3, stop_tone/2, get_tone_sender/1]).


start_tsu(Address) ->
	Id = spawn_link(tone, tsu, [Address, 8]), % 8 = hard coded number
	register(tone_sender, Id).

tsu(Address, Number) ->
	process_flag(trap_exit, true),
	subscribe(Address, Number),
	lim_driver:send_cmd(Address, {activate_tsu, Number, 0}), % type = 0
        lim_driver:send_cmd(Address, {market_info, 2}),  % England tones
	receive
	    {lim_driver, {Address, {identity, tsu, active, _}}} ->
		io:format('TSU at ~w started~n', [Address])
	 
	    after 10000 ->
		io:format('TSU at ~w not started- timed out~n', [Address]),
		exit(did_not_start_tsu)
	 end,
	top(Address).


subscribe(_Address, 0) -> 
	true;
subscribe(Address, N) ->
	lim_driver:subscribe(Address + N - 1, driver_tsu),
	subscribe(Address, N - 1).

% the above contains the hard coded information about how to send which
% tone.
top(Address) ->
    receive
	{From, start_tone, Tone, Tone_Ad} ->
	    case Tone of
	        dial ->
		    lim_driver:send_cmd(Tone_Ad, {start_tone, 18, 0});
	        ring ->
		    lim_driver:send_cmd(Tone_Ad, {start_tone, 18,21});
	        busy ->
		    lim_driver:send_cmd(Tone_Ad, {start_tone, 18, 3});
	        fault ->
		    lim_driver:send_cmd(Tone_Ad, {start_tone, 18,6});
	        _     ->
	            exit(From, 'bad_tone_request')
	    end;
	{_From, stop_tone, Tone_Ad} ->
	    lim_driver:send_cmd(Tone_Ad, {start_tone, 0, 0});
	{'EXIT',_,system_exit} ->
	    io:format('Tone mangager terminating~n', []),
	    exit(normal);
	{'EXIT', _From, Reason} ->
	    io:format('Tsu got Exit ~w~n', [Reason]);
	Other ->
	    io:format('Tsu got message ~w~n', [Other])
    end,
    top(Address).

% this routine should be replaced by a proper allocator

get_tone_sender(Line) ->
    {Min, Max, OffSet} = configure:get_tone_sender_info(),
	if
	    Line > Min, Line < Max ->		
	        Line + OffSet;			
	    true -> 
	        exit('bad_address_to_which_to_send_tone')
	end.

start_tone(_Line, Tone, Tone_Ad) ->
    %% (patric)
    %% Done in switch instead.
    %% Tone_Ad = get_tone_sender(Line),
    tone_sender ! {self(), start_tone, Tone, Tone_Ad}.
    %% switch:connect(Line, Tone_Ad).    


stop_tone(_Line,Tone_Ad) ->
    %% (patric)
    %% Done in switch instead.
    %% Tone_Ad = get_tone_sender(Line),
    %% switch:disconnect(Line, Tone_Ad),
    tone_sender ! {self(), stop_tone, Tone_Ad}.
