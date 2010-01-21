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
