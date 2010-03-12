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
%% File    : driver_lib.erl
%% Author  : Robert Virding
%% Purpose : Application specific device driver functions.
%%  
%%    The function unit/1 is required by the generic driver to take a signal
%%    and return some application specific unit. It is of course perfectly
%%    legal only to have ONE unit. All other functions defined here are
%%    optional.
%%  
%%    The other functions defined here are just general utilities for decoding/
%%    encoding MD110 hardware signals.

-module(driver_lib).
-export([unit/1]).	%Must export this
-export([bits/3, decode/1, decode_identity/1, encode/4]).

%% Unit = unit(SignalBytes)
%%  Return a (application specific) unit. The 2 first bytes contain
%%  Type (4 bits), Flags (2 bits), and Multiple (10 bits).

unit([Msb,Lsb|_]) ->
	%Check the type field the h/w interface defines
	% these special values.
	case Msb bsr 4 of
	    9 ->
		bsu;
	    15 ->
		bsu;
	    _ ->
		Msb band 3 bsl 8 bor Lsb
	end.

%% Value = bits(First, Last, Integer)
%%  Bits are numbered 0..7 I < J (0 is low order bit)

bits(I,I,X) ->	%Special case some calls
	(X bsr I) band 1;
bits(0,J,X) ->
	X band (2 bsl J - 1);
bits(I,J,X) ->
	(X bsr I) band (1 bsl (J - I + 1) - 1).

%% {Type,Flags,Multiple,Bytes} = decode(Signal)
%%  Decode a signal into the type, flags, multiple and actual signal bytes.

decode([Msb,Lsb|Bytes]) ->
	Type = Msb bsr 4,
	Flags = (Msb bsr 2) band 3,
	Mult = Msb band 3 bsl 8 bor Lsb,
	{Type,Flags,Mult,Bytes}.

%% Signal = encode(Type, Flags, Multiple, Bytes)
%%  Take type, flags, multiple and actual signal bytes and encode a signal.

encode(Type,Flags,Mult,Bytes) ->
	Msb = Type bsl 4 bor (Flags bsl 2) bor (Mult bsr 8),
	Lsb = Mult band 255,
	[Msb,Lsb|Bytes].

%% Identity = decode_identity(IndentityBytes)
%%  Return an indentity structure derived from the relevant bytes.

decode_identity([D02,D03]) ->
	Type = identity_type(D02),
	State = identity_state(bits(7,7,D03)),
	Ndev = bits(0,6,D03) + 1,
	{identity,Type,State,Ndev}.

identity_type(2) ->	elu2;
identity_type(5) ->	elu5;
identity_type(6) ->	mpu;
identity_type(13) ->	tru1;
identity_type(14) ->	tsu;
identity_type(16) ->	iou5;
identity_type(27) ->	tlu20;
identity_type(31) ->	elu5;
identity_type(36) ->	gjul;
identity_type(49) ->	tru2.

identity_state(0) ->	passive;
identity_state(1) ->	active.

