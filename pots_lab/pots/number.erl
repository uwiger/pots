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
%% Author:  Patric Pramdal
%% Purpose: Erlang Intro Course

-module(number).
-export([start/0,store/2,valid_sequences/0,analyse/2]).

-export([a_analyse/2]).

start()->ok.
store(_,_)->ok.

valid_sequences()->
    [1,[3,
	2,[3,
	   5]],
     2,[4,[7]]].

analyse(D,[D,Ds|_]) when list(Ds)->
    {incomplete,Ds};
analyse(D,[D|_])->
    valid;
analyse(D,[_|Ds])->
    analyse(D,Ds);
analyse(_,[])->
    invalid.


a_analyse(Digit, Sequences) ->
    ReplyTo = self(),
    spawn(fun() ->
		  Result = analyse(Digit, Sequences),
		  ReplyTo ! {number, {analysis_reply, {Digit, Result}}}
	  end).
