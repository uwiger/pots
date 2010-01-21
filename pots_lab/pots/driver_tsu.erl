-module(driver_tsu).
-export([decode/1, encode/1]).

decode([22|Bytes]) ->
	driver_lib:decode_identity(Bytes).

%%  Signal encoding entry point.

encode({activate_tsu,Ndev,KeyCadence}) ->
	N = Ndev - 1,
	[21,N,KeyCadence];
encode({start_tone,Freq,Cadence}) ->
	[1,Freq,Cadence];
encode(bordid) ->
	[22];
encode({market_info,Code}) ->
	[7,Code].

