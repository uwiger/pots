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
-module(driver_dts).
-export([decode/1, encode/1]).

decode([1,_,D03,D04,D05|_]) ->
	Version = equsta_ver(D03),
	LineStatus = equsta_lin(D04),
	Equipment = equsta_equ(D05),
	{dts_state,Version,LineStatus,Equipment};
decode([2,D02]) ->
	{key_pressed,{program,D02}};
decode([4,X]) ->
	fixfncact(X);
decode([22|Bytes]) ->
	driver_lib:decode_identity(Bytes).

%% Decoding support functions.

equsta_ver(0) -> mini;
equsta_ver(1) -> midi;
equsta_ver(2) -> maxi;
equsta_ver(3) -> standard.

equsta_lin(0) -> on_hook;
equsta_lin(1) -> off_hook.

equsta_equ(0) -> reset;
equsta_equ(1) -> idle;
equsta_equ(2) -> low_speaking;
equsta_equ(3) -> loud_speaking.

fixfncact(D) when D >= 0, D =< 9 -> {key_pressed,{digit,D}};
fixfncact(10) -> {key_pressed,star};
fixfncact(11) -> {key_pressed,hash};
fixfncact(12) -> {key_pressed,transfer};
fixfncact(13) -> {key_pressed,clear};
fixfncact(14) -> {key_pressed,loud};
fixfncact(15) -> on_hook;
fixfncact(16) -> off_hook.

%% Signal encoding entry point.

encode({activate_dts,
		{intern_ring,MIRon1,MIRoff1,MIRon2,MIRoff2},
		{extern_ring,MERon1,MERoff1,MERon2,MERoff2},
		{callback_ring,MCBon1,MCBoff1,MCBon2,MCBoff2},
		{pulse_flash,MPFon1,MPFoff1,MPFon2,MPFoff2},
		{slow_flash,MSFon,MSFoff},{fast_flash,MFFon,MFFoff}}) ->
	%% Erlang difference lists (but backwards)!
	Ff = equactdts([MFFon,MFFoff],10,[]),
	Sf = equactdts([MSFon,MSFoff],10,Ff),
	Pf = equactdts([MPFon1,MPFoff1,MPFon2,MPFoff2],10,Sf),
	Cr = equactdts([MCBon1,MCBoff1,MCBon2,MCBoff2],50,Pf),
	Er = equactdts([MERon1,MERoff1,MERon2,MERoff2],50,Cr),
	Ir = equactdts([MIRon1,MIRoff1,MIRon2,MIRoff2],50,Er),
	[128|Ir];
encode({set_transmission,Type}) ->
	[132,distrmupdate(Type)];
encode(clear_display) ->
	[133,0];
encode({display,CharList}) ->
	[135|CharList];
encode({start_ring,Type,Version}) ->
	[indcalupdate(Type,Version),0];
encode(stop_ring) ->
	[142,0];
encode({clear_indicator,all}) ->
	[143,0];
encode({clear_indicator,Ind}) ->
	[144,Ind];
encode({set_indicator,Ind,Type}) ->
	[indcalupdate(Type),Ind];
encode({activate_elu5,8}) ->
	[21,0,0];
encode({activate_elu5,16}) ->
	[21,0,1];
encode(bordid) ->
	[22].

equactdts([],_,Tail) ->
	Tail;
equactdts([H|T],Unit,Tail) ->
	[H div Unit|equactdts(T,Unit,Tail)].

distrmupdate(no_speaking) ->	0;
distrmupdate(low_speaking) ->	1;
distrmupdate(load_speaking) ->	2.

indcalupdate(intern,normal)      -> 136;
indcalupdate(intern,first_low)   -> 137;
indcalupdate(extern,normal)      -> 138;
indcalupdate(extern,first_low)   -> 139;  % UW 100313: s/intern/extern
indcalupdate(callback,normal)    -> 140;
indcalupdate(callback,first_low) -> 141.
indcalupdate(steady) 		 -> 145;
indcalupdate(pulse_flash) 	 -> 146;
indcalupdate(slow_flash) 	 -> 147;
indcalupdate(fast_flash) 	 -> 148.

