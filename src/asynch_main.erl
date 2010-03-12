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
-module(asynch_main).

-export([event_loop/2,
	 event_loop/3]).

-include("messages.hrl").


%% simple event loop with FIFO semantics
event_loop(M, S) ->
    receive
	{From, Event} ->
	    dispatch(From, Event, M, S);
	{From, Ref, Event} ->
	    dispatch(From, Event, M, S);
	Other ->
	    io:format("event_loop received unknown msg: ~p~n", [Other]),
	    exit({unknown_msg, Other})
    end.


event_loop(M, S, Recv) ->
    %% Recv is a tuple, where each element represents a filter.
    %% From is used to locate the right filter, and the value of 
    %% the filter in that position is interpreted as follows:
    %%    false : ignore (buffer) message
    %%    []    : consume message
    %%    Value : consume iff Ref == Value
    receive
	{From, Event} when element(From, Recv) == [] ->
	    dispatch(From, Event, M, S);
	{From, Ref, Event} when element(From, Recv) == Ref ->
	    dispatch(From, Event, M, S);
	{From, Ref, Event} when element(From, Recv) == [] ->
	    dispatch(From, Event, M, S)
    end.


dispatch(From, Event, M, S) when atom(Event) ->
    handle(M:Event(From, S), M);
dispatch(From, {Event, Arg}, M, S) ->
    handle(M:Event(From, Arg, S), M).

handle({ok, NewState}, M) ->
    event_loop(M, NewState);
handle({ok, NewState, Recv}, M) ->
    event_loop(M, NewState, Recv).
