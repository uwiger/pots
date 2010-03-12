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
-module(lim_asynch).

-export([start_tone/1,
	 stop_tone/0,
	 start_ringing/0,
	 stop_ringing/0,
	 connect_to/1,
	 disconnect_from/1,
	 pid_with_telnr/1]).

-include("messages.hrl").

start_tone(Type) when Type==dial; Type==fault; Type==ring; Type==busy ->
    send_asynch({start_tone, Type}).
stop_tone() ->		send_asynch(stop_tone).
start_ringing() ->	send_asynch(start_ringing).
stop_ringing() ->	send_asynch(stop_ringing).
connect_to(Pid) ->	send_asynch({connect_to, Pid}).
disconnect_from(Pid) ->	send_asynch({disconnect_from, Pid}).
pid_with_telnr(N) ->    send_asynch({pid_with_telnr, N}).

send_asynch(Req) ->
    Ref = make_ref(),
    lim ! {request, Req, Ref, self()},
    Ref.
