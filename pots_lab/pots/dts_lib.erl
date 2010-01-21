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
%% This file is a very simple library of functions
%% used by the dts (Digital Telephone Set)
%% control process for use in Erlang courses. It contains
%% hard coded data for which would normally be stored in
%% a configuration data base. 
%%
%% Irresponsible author - Mike Williams

-module(dts_lib).

-export([dts_init/1,dts_start/1]).

dts_init(Address) ->
	lim_driver:send_cmd(Address, {activate_dts,
                {intern_ring, 1000,5000,1000,5000},
                {extern_ring, 500,50,500,4950},
                {callback_ring, 200,50,200,50},
                {pulse_flash, 100,100,100,500},
                {slow_flash,400,400},
                {fast_flash,200,200}}),
	lim_driver:send_cmd(Address, clear_display),
	lim_driver:send_cmd(Address, { clear_indicator,all}),
	lim_driver:send_cmd(Address, stop_ring),
	lim_driver:send_cmd(Address, {set_transmission, low_speaking}).

dts_start(Address) ->
    %% (patric)
    %% Let all msg:s go through switch instead. (See switch.erl)
    %% UW: replaced switch.erl with lim.erl
    %% lim_driver:subscribe(Address, driver_dts),
    lim_driver ! {whereis(lim), subscribe, Address, driver_dts},
    
    %% (patric)
    %% Done in switch. (because of above)
    %% dts_init(Address),
    lim ! {self(),{dts_init,Address}},

    %% (patric)
    %% Let switch keep track of this information. (See switch.erl)
    %% proc_reg:putp(Address),
    
    %% (patric)
    %% Just renamed it, and it doesn't need to know about Address anymore.
    %% The switch knows all about addresses, pids & telephone numbers.
    control:start().
