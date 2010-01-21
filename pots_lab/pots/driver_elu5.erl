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
-module(driver_elu5).
-export([decode/1, encode/1]).

%% the elu5 is also a dts !!! 

decode(Bytes) -> apply(driver_dts,decode,[Bytes]).

encode({activate_elu5,8})  ->	[21,0,0];
encode({activate_elu5,16}) ->	[2,0,1];
encode(bordid) 		   ->	[22];
encode(Signal) 		   ->	apply(driver_dts,encode,[Signal]).

