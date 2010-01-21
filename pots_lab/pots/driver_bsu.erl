-module(driver_bsu).
-export([decode/1, encode/1]).


%%    Signal decoding entry points.
%%  
%%    We recognise the special coding of 'reset' here. This VERY dependant on
%%    the specifics of the hardware interface.

decode([255,255,0]) -> %RESET signal
	reset;
decode([_,_|Bytes]) ->
	decode_bytes(Bytes).

%% Signal = decode_bytes(ByteList)
%%  This does all the work of decoding a signal.

decode_bytes([12,D02,D03,D04]) ->
	%% BSU ULT - signal sent from lim switch when a fault occurs
	%% on a time slot.
	Multiple = D02 bsl 8 bor D03,
	FaultCode = D04,
	{switch_fault,Multiple,FaultCode};
decode_bytes([13|Bytes]) ->
	undefined.

%% Signal encoding entry point.

encode(reset) ->	%RESET signal
	[255,255,0];
encode(Signal) ->
	driver_lib:encode(9,0,0,encode_signal(Signal)).

encode_signal({connect,A,Na,B,Nb}) ->
	%% Connect A to B. 
	%% Na is the number of listeners to A (after the operation)
	%% Nb is the number of listeners to B (after the operation)
	%%
	D02 = 15,
	{D03,D04} = pack_multiple(A),
	%% In the first call More = 1 (i.e. another packet follows)
	%% OW = 1 , Attn = 1, Con = 1 (want a connection)
	{D05,D06} = pack_bsu_sender(1,1,1,1,B),
%	D07 = Na,
	D07 = Nb,
	{D08,D09} = pack_multiple(B),
	%% in the second call More = 0 - this is the last packet in the frame
	{D10,D11} = pack_bsu_sender(0,1,1,1,A),
%	D12 = Nb,
	D12 = Na,
	[D02,D03,D04,D05,D06,D07,D08,D09,D10,D11,D12,0,0,0,0,0];
encode_signal({disconnect,A,Na,B,Nb}) ->
	%% Connect A to B. 
	%% Na is the number of listeners to A (after the operation)
	%% Nb is the number of listeners to B (after the operation)
	D02 = 15,
	{D03,D04} = pack_multiple(A),
	%% In the first call More = 1 (i.e. another packet follows)
	%% OW = 1 , Attn = 1, Con = 1 (want a connection)
	%% Con = 0 don't want a connection
	{D05,D06} = pack_bsu_sender(1,1,1,0,B),
%	D07 = Na,
	D07 = Nb,
	{D08,D09} = pack_multiple(B),
	%% In the second call More = 0 - this is the last packet in the frame
	{D10,D11} = pack_bsu_sender(0,1,1,0,A),
%	D12 = Na,
	D12 = Nb,
	[D02,D03,D04,D05,D06,D07,D08,D09,D10,D11,D12,0,0,0,0,0];
encode_signal({start_listen,To,From,Nlist}) ->
	%% To listens to From
	%% NList is the number of listeners  (after the operation)
	D02 = 15,
	{D03,D04} = pack_multiple(To),	% the receiver
	%% In the first call More = 0 (i.e. no more packets follow)
	%% OW = 1 , Attn = 1, Con = 1 (want a connection)
	{D05,D06} = pack_bsu_sender(0,1,1,1,From),
	D07 = Nlist,
	[D02,D03,D04,D05,D06,D07,0,0,0,0,0,0,0,0,0,0];
encode_signal({stop_listen,To,From,Nlist}) ->
	%% Same as start_listen BUT Con = 0
	%% To listens to From
	%% NList is the number of listeners  (after the operation)
	D02 = 15,
	{D03,D04} = pack_multiple(To),	% the receiver
	%% In the first call More = 0 (i.e. no more packets follow)
	%% OW = 1 , Attn = 1, Con = 0 (don't want a connection)
	{D05,D06} = pack_bsu_sender(0,1,1,0,From),
	D07 = Nlist,
	[D02,D03,D04,D05,D06,D07,0,0,0,0,0,0,0,0,0,0];
encode_signal(reset_switch) ->
	[14];
encode_signal(start_switch) ->
	[7].

%% {Byte1,Byte2} = pack_multiple(Multiple)

pack_multiple(Mult) ->
	Byte1 = Mult bsr 8,
	Byte2 = Mult band 255,
	{Byte1,Byte2}.
%% {Byte1,Byte2} = pack_bsu_sender(More, OverWrite, Attenutation, Multiple)
%%
%%  Pack two bytes as follows:
%%		+-----+----+----+----+----+-----+----+----+
%%   	Byte1  	|More | OW | Attenuation  | Con |    M    |
%%		+-----+----+----+----+----+-----+----+----+
%%   	Byte2  	|  u     l    t   i    p     l    e       |
%%		+-----+----+----+----+----+-----+----+----+
%%		   7     6    5    4    3    2     1    0
%%
%%	More = 1 if more bytes are to follow
%%	OW   = 1 if overwriting sample in time slot buffer
%%	Attn = 0..7 (Attenuation)
%%	Con  = 0 disconnect
%%	Mult = 10 bit multiple value
%%
%%	We assume called values are correct (i.e. OW is 0 or 1 etc).

pack_bsu_sender(More,OW,Attn,Con,Mult) ->
	Byte1 = More bsl 7 bor (OW bsl 6) bor (Attn bsl 3) 
	             bor (Con bsl 2) bor (Mult bsr 8),
	Byte2 = Mult band 255,
	{Byte1,Byte2}.


