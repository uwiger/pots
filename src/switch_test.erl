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
-module(switch_test).
-export([run/0]).

run()->
    control_lib:print("TESTING start tone"),
    control_lib:start_tone(dial),

    control_lib:print("TESTING start tone when already started"),
    control_lib:start_tone(fault),

    control_lib:print("TESTING stop tone"),
    control_lib:stop_tone(),

    control_lib:print("TESTING stop tone when not started"),
    control_lib:stop_tone(),

    control_lib:print("TESTING connect"),
    control_lib:connect_to(switch:pid_with_telnr(13)),

    control_lib:print("TESTING start tone when connected"),
    control_lib:start_tone(busy),

    control_lib:print("TESTING stop tone when connected"),
    control_lib:stop_tone(),

    %% Connect

    control_lib:print("TESTING connect when already connected"),
    control_lib:connect_to(switch:pid_with_telnr(13)),

    control_lib:print("TESTING disconnect"),
    control_lib:disconnect_from(switch:pid_with_telnr(13)),

    control_lib:print("TESTING disconnect when not connected"),
    control_lib:disconnect_from(switch:pid_with_telnr(13)),

    control_lib:print("TESTING connect when tone started"),
    control_lib:start_tone(busy),
    control_lib:connect_to(switch:pid_with_telnr(13)),

    control_lib:print("TESTING disconnect when tone started"),
    control_lib:disconnect_from(switch:pid_with_telnr(13)),

    control_lib:print("TESTING ringing when tone started"),
    control_lib:start_ringing().
