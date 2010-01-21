-module(simulator).

-export([start/2,sim/2,phone/1,ring/0,tone/2,rectangle/3]).
-record(phone,{onoff,x,y,poly,line,rec,digs,butts,
	       num,mult,seq,ring,ring1,ring2}).

%%===========================================================================
%%
%% Graphics LIM simulator for use for the Erlang course
%%
%%===========================================================================

start(sim,Subs) ->
    register(simulator,spawn_link(simulator,sim,[sim,Subs]));
start(both,Subs) ->
    register(simulator,spawn_link(simulator,sim,[both,Subs]));
start(_,_) -> true.

sim(Type,Subs) ->    
    S = gs:start(),
    Window = gs:create(window,S,[{width,600}, {height,415},{title,"Telephony"}]),
    Canvas = gs:create(canvas,Window,[{height,415},{width,600}]),
    mk_switch(Canvas),
    Destroy = gs:create(button,Window,[{label,{text, "Destroy"}}]),
    Phones = mk_phone(Canvas, Window, [220,15, 75,80, 75,230, 220,270],Subs,1),
    Tones = mk_tone(Canvas, 160,1),
    gs:config(Window,{map,true}),
    loop(Type,Destroy,Phones,Tones).

%%===========================================================================
%%
%% Main loop receiving messages to/from the LIM hardware
%%
%%===========================================================================

loop(Type,Destroy,Phones,Tones) ->
    receive 
	{from_hw,Mult,off_hook} ->
	    send_to_all({Mult,off_hook},Phones),
	    loop(Type,Destroy,Phones,Tones);
	{from_hw,Mult,on_hook} ->	
	    send_to_all({Mult,on_hook},Phones),
	    loop(Type,Destroy,Phones,Tones);
	{from_hw,Mult,{key_pressed,{digit,Digit}}} ->
	    send_to_all({Mult,digit,Digit},Phones),
	    loop(Type,Destroy,Phones,Tones); 
	{from_hw,Mult,{key_pressed,hash}} ->
	    send_to_all({Mult,digit,10},Phones),
	    loop(Type,Destroy,Phones,Tones); 
	{from_hw,Mult,{key_pressed,star}} ->
	    send_to_all({Mult,digit,11},Phones),
	    loop(Type,Destroy,Phones,Tones); 
	{to_hw,Mult,{start_ring,_,_}} ->
	    send_to_all({Mult,start_ring},Phones),
	    loop(Type,Destroy,Phones,Tones);
	{to_hw,Mult,stop_ring} ->
	    send_to_all({Mult,stop_ring},Phones),
	    loop(Type,Destroy,Phones,Tones);
	{to_hw,Mult,{start_tone,A,B}} ->
	    send_to_all({Mult,tone,A,B},Tones),
	    loop(Type,Destroy,Phones,Tones);
	{to_hw,bsu,{connect,Mult1,_,Mult2,_}} ->
	    rectangle ! {connect,Mult1,Mult2},
	    loop(Type,Destroy,Phones,Tones);
	{to_hw,bsu,{disconnect,Mult1,_,Mult2,_}} ->
	    rectangle ! {disconnect,Mult1,Mult2},
	    loop(Type,Destroy,Phones,Tones);
	{to_hw,Mult,{activate_elu5,N}} when Type == sim ->
	    lim_driver ! {simulator,{Mult,{identity,elu5,active,N}}},
	    loop(Type,Destroy,Phones,Tones);
	{to_hw,Mult,{activate_tsu,N,M}} when Type == sim ->
	    lim_driver ! {simulator,{Mult,{identity,tsu,active,N}}},
	    loop(Type,Destroy,Phones,Tones);
	{gs,Destroy, click,_,_} ->
	    unregister(simulator),
	    io:format("Simulator terminating~n",[]),
	    unregister(rectangle),
	    exit(simulator_finished);
	{gs,S, click,_,_} when Type == sim ->
	    send_to_all(S,Phones),
	    loop(Type,Destroy,Phones,Tones);
	_ -> loop(Type,Destroy,Phones,Tones)
    end.

send_to_all(S,[]) -> [];
send_to_all(S,[P|L]) ->
    P ! S,
    send_to_all(S,L).

%%===========================================================================
%%
%% Subroutine to draw the telephones
%%
%%===========================================================================

mk_phone(_,_,[],_,_) -> [];
mk_phone(C,W,[X,Y|T],[{Seq,Mult}|M],N) ->
    gs:create(line,C,[{width,1},{coords,[{X,Y+66},{340,100+N*30}]}]),
    gs:create(rectangle,C,[{coords,[{X-30,Y+27},{X+30,Y+115}]},
                           {bw,1},{fg,black},{fill,cyan}]),
    gs:create(text,C,[{coords,[{X,Y+109}]},{anchor, center},{text,ascii(Seq)}]),
    rectangle ! {phone,Mult,100+N*30},
    Phone = spawn_link(simulator,phone,
		       [#phone{onoff = on,
			       x = X, 
			       y = Y,
			       poly = gs:create(polygon,C,
						[{fg,cyan}, {fill, cyan},
						 {coords,receiver(X,Y)}]),
			       line = gs:create(line,C,
						[{fg,black},
						 {coords,receiver(X,Y)}]),
			       rec = gs:create(button,W,
					       [{x,X-7},{y,Y+5},{bg,cyan},
						{width,18},{height,18}]),
			       digs = gs:create(text,C,[{coords,[{X,Y+125}]},
						        {anchor, center}]),
			       butts =  reorg(mk_buttons(W,[["1","2","3"],
							    ["4","5","6"],
							    ["7","8","9"],
							    ["*","0","#"]],
							 X-25,Y+32)),
			       num = [],
			       mult = Mult,
			       seq = Seq,
			       ring = [],
			       ring1 = gs:create(line,C,
						 [{width,1},{fg,black}]),
			       ring2 = gs:create(line,C,
						 [{width,1},{fg,black}])}]),
    [Phone|mk_phone(C,W,T,M,N+1)].

%%===========================================================================
%%
%% Record type 'phone'
%% onoff = on|off; x,y = position; poly,line,rec = gs receiver and button;
%% digs = gs for digits; butts = gs for the 12 buttons; num = dialled no;
%% mult = multiple; seq = own number; ring = ring process pid;
%% ring1,ring2 = drawn lines when ringing
%%
%%===========================================================================

ascii([]) -> [];
ascii([H|T]) -> [H+$0|ascii(T)].

reorg([A,B,C,D,E,F,G,H,I,Star,J,Hash]) ->
    [J,A,B,C,D,E,F,G,H,I,Hash,Star].

mk_buttons(_,[],_,_) -> [];
mk_buttons(W,[L|T],X,Y) ->
    lists:append(mk_button(W,L,X,Y),mk_buttons(W,T,X,Y+18)).

mk_button(_,[],_,_) -> [];
mk_button(W,[B|T],X,Y) ->
    [gs:create(button,W,[{label,{text, B}},{x,X},{y,Y},{bg,cyan},
			 {width,18},{height,18}]) |
     mk_button(W,T,X+18,Y)].

receiver(X,Y) ->    delta(X,Y,[12,0, 36,6, 54,15, 60,24,
			      63,33, 33,33, 36,24,
			      -36,24, -33,33, -63,33,
			      -60,24, -54,15, -36,6, -12,0,
			      12,0]).

delta(X,Y,[DX,DY|T]) -> 
    [{X+DX,Y+DY}|delta(X,Y,T)];
delta(_,_,[]) -> [].


%%===========================================================================
%%
%% Telephone loop
%%
%%===========================================================================

phone(P) ->
    Onoff = P#phone.onoff,
    Rec = P#phone.rec,
    Mult = P#phone.mult,
    receive
	{Mult,off_hook} when Onoff == on -> 
	    gs:config(P#phone.poly,{move,{0,-10}}),
	    gs:config(P#phone.line,{move,{0,-10}}),
	    gs:config(P#phone.rec,{y,P#phone.y-5}),
	    gs:config(P#phone.digs,{text," "}),
	    phone(P#phone{onoff=off,num=[]});
	{Mult,on_hook} when Onoff == off -> 
	    gs:config(P#phone.poly,{move,{0,10}}),
	    gs:config(P#phone.line,{move,{0,10}}),
	    gs:config(P#phone.rec,{y,P#phone.y+5}),
	    gs:config(P#phone.digs,{text," "}),
	    phone(P#phone{onoff=on,num=[]});
	{Mult,start_ring} -> 
	    case P#phone.ring of
		[] -> gs:config(P#phone.digs,{text,"!! RING !! "}),
		      gs:config(P#phone.ring1,
				{coords,ringline(P#phone.x,P#phone.y-10)}),
		      gs:config(P#phone.ring2,
				{coords,ringline(P#phone.x,P#phone.y-20)}),
		      phone(P#phone{ring = spawn_link(simulator,ring,[])});
		_ -> phone(P)
	    end;
	{Mult,stop_ring} -> 
	    case P#phone.ring of
		[] -> phone(P);
		Ring -> gs:config(P#phone.digs,{text," "}),
			X = P#phone.x,
			Y = P#phone.y + 27,
			gs:config(P#phone.ring1,{coords,[{X,Y},{X,Y}]}),
			gs:config(P#phone.ring2,{coords,[{X,Y},{X,Y}]}),
			Ring ! stop,
			phone(P#phone{ring=[]})
	    end;
	{Mult,digit,Digit} -> 
	    Dig = lists:nth(Digit+1,["0","1","2","3","4","5",
				     "6","7","8","9","#","*"]),
	    N1 = new_digit(P#phone.num,Dig),
	    gs:config(P#phone.digs,{text,N1}),
	    phone(P#phone{num=N1});
	Rec when Onoff == on -> 
	    lim_driver ! {simulator,{Mult,off_hook}},
	    simulator ! {from_hw,Mult,off_hook},
	    phone(P);
	Rec when Onoff == off -> 
	    lim_driver ! {simulator,{Mult,on_hook}},
	    simulator ! {from_hw,Mult,on_hook},
	    phone(P);
	S -> case match(S,P#phone.butts,0) of
		 [] -> phone(P);
		 10 -> lim_driver ! {simulator,{Mult,{key_pressed,hash}}},
		       simulator ! {from_hw,Mult,{key_pressed,hash}},
		       phone(P);
		 11 -> lim_driver ! {simulator,{Mult,{key_pressed,star}}},
		       simulator ! {from_hw,Mult,{key_pressed,star}},
		       phone(P);
		 N ->  lim_driver ! {simulator,{Mult,{key_pressed,{digit,N}}}},
		       simulator ! {from_hw,Mult,{key_pressed,{digit,N}}},
		       phone(P)
	     end
    end.

match(_,[],_) -> [];
match(S,[S|B],N) -> N;
match(S,[_|B],N) -> match(S,B,N+1).

new_digit(N,M) ->
    if length(N) > 8 -> N;
	true -> lists:append(N,M)
    end.

ring() ->
    % unix:cmd("cat /usr/demo/SOUND/sounds/ring.au > /dev/audio"),
    receive
	_ -> exit(normal)
    after 5000 ->
	    ring()
    end.

ringline(X,Y) -> delta(X,Y,[54,15, 36,6, 12,0, -12,0, -36,6, -54,15]).

%%===========================================================================
%%
%% Subroutine to draw the tone senders
%%
%%===========================================================================

mk_tone(_,_,N) when N > 4 -> [];
mk_tone(C,Mult,N) ->
    Y = N*30,
    gs:create(line,C,[{width,1},{coords,[{440,100+Y}, {500,100+Y}]},
		      {fg,black}]),
    gs:create(rectangle,C,[{coords,[{500,100+Y-8}, {560,100+Y+8}]},
			   {bw,1},{fg,black},{fill,cyan}]),
    rectangle ! {tone,Mult,100+Y},
    Tone = spawn(simulator,tone,
		 [Mult,gs:create(text,C,[{coords,[{530,100+Y}]}, {anchor, center}])]),
    [Tone|mk_tone(C,Mult+1,N+1)].

%%===========================================================================
%%
%% Tone sender loop
%%
%%===========================================================================

tone(Mult,T) ->
    receive
	{Mult,tone,18,0} -> gs:config(T,{text,"dial"});
	{Mult,tone,18,21} -> gs:config(T,{text,"ring"});
	{Mult,tone,18,3} -> gs:config(T,{text,"busy"});
	{Mult,tone,18,6} -> gs:config(T,{text,"error"});
	{Mult,tone,_,_} -> gs:config(T,{text," "});
	_ -> true
    end,
    tone(Mult,T).

%%===========================================================================
%%
%% Subroutine to draw the switch rectangle
%%
%%===========================================================================

mk_switch(C) -> gs:create(rectangle,C,[{coords,[{340,100}, {440,300}]},
				       {bw,3},{fg,black}]),
		gs:create(text,C,[{coords,[{390,290}]},{text,"SWITCH"}, {anchor, center}]),
		register(rectangle,spawn_link(simulator,rectangle,[C,[],[]])).

%%===========================================================================
%%
%% Switch rectangle loop
%%
%%===========================================================================

rectangle(C,Phones,Tones) ->
    receive
	{phone,Mult,Y} -> rectangle(C,[{Mult,Y,
					gs:create(arc,C,[{bw,1}])}|Phones],
				    Tones);
	{tone,Mult,Y} -> rectangle(C,Phones,
				   [{Mult,Y,
				     gs:create(line,C,[{width,1}])}|Tones]);
	{connect,Mult1,Mult2} ->
	    connect_phones(find(Mult1,Phones),find(Mult2,Phones)),
	    connect_phone_tone(find(Mult1,Phones),find(Mult2,Tones)),
	    rectangle(C,Phones,Tones);
	{disconnect,Mult1,Mult2} ->
	    disconnect_phones(find(Mult1,Phones),find(Mult2,Phones)),
	    disconnect_phone_tone(find(Mult1,Phones),find(Mult2,Tones)),
	    rectangle(C,Phones,Tones);
	_ -> rectangle(C,Phones,Tones)
    end.

find(Mult,[]) -> [];
find(Mult,[{Mult,Y,L}|_]) -> {Y,L};
find(Mult,[_|P]) -> find(Mult,P).

connect_phones([],_) -> true;
connect_phones(_,[]) -> true;
connect_phones({Y1,L},{Y2,M}) when Y2 > Y1 -> 
    connect_phones({Y2,M},{Y1,L});
connect_phones({Y1,L},{Y2,M}) ->
    A = (Y1-Y2) div 2,
    gs:config(L,[{coords,[{340-A,Y1}, {340+A,Y2}]},{start,270},{extent,180}]).

connect_phone_tone([],_) -> true;
connect_phone_tone(_,[]) -> true;
connect_phone_tone({Y1,_},{Y2,M}) -> gs:config(M,[{coords,[{340,Y1}, {440,Y2}]}]).

disconnect_phones([],_) -> true;
disconnect_phones(_,[]) -> true;
disconnect_phones({Y1,L},{Y2,M}) ->
    gs:config(L,[{coords,[{340,Y1}, {340,Y1}]},{start,270},{extent,0}]),
    gs:config(M,[{coords,[{340,Y2}, {340,Y2}]},{start,270},{extent,0}]).

disconnect_phone_tone(_,[]) -> true;
disconnect_phone_tone(_,{Y2,M}) -> gs:config(M,[{coords,[{440,Y2}, {440,Y2}]}]).






