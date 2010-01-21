-module(driver_elu5).
-export([decode/1, encode/1]).

%% the elu5 is also a dts !!! 

decode(Bytes) -> apply(driver_dts,decode,[Bytes]).

encode({activate_elu5,8})  ->	[21,0,0];
encode({activate_elu5,16}) ->	[2,0,1];
encode(bordid) 		   ->	[22];
encode(Signal) 		   ->	apply(driver_dts,encode,[Signal]).

