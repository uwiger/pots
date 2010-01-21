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
