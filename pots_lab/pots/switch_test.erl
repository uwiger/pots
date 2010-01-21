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
