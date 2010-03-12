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

