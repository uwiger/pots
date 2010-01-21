%% Copyright (C) 1993, Ellemtel Telecommunications Systems Laboratories
%% File    : configure.erl
%% Author  : Joe + Tobbe
%% Purpose : Configuration data for LIm + Mini_lim

-module(configure).

-export([limdev/0, back/0, front/0, subscribers/0, elu_start_address/0,
	 get_tone_sender_info/0]).

-import(lists, [append/2]).

%% the following two functions are all that you should need to change
%% version() -> su | mini_lim
%% root()    -> "root directory of the pots exercise"

%version() -> su.
version() -> mini_lim.

root()    -> "/home/erlang/courses/intro3.2/pots".

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% You shouldn't need to change below this line %%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

back()   -> append(root(), "/back").
limdev() -> list_to_atom(append(root(), "/limdev /dev/ttya")).
front()  -> append(root(), "/front").

elu_start_address() -> elu_start_address(version()).

elu_start_address(su) -> 64;
elu_start_address(mini_lim) -> 32.

subscribers() -> subscribers(version()).

subscribers(su) ->
    [{[1,2,3],64},
     {[1,2,5],65},
     {[1,3],66},
     {[2,4,7],67}];
subscribers(mini_lim) ->
    [{[1,2,3],32},
     {[1,2,5],33},
     {[1,3],34},
     {[2,4,7],35}].
		
get_tone_sender_info() ->  get_tone_sender_info(version()).

get_tone_sender_info(su)       -> {63, 72, 96};
get_tone_sender_info(mini_lim) -> {31, 40, 128}.
