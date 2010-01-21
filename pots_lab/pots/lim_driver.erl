%% Lim Driver
%%
%% 1991-01-10
%%
%% Authors: Origional author Robert Virding. Modified and simplified by Joe 
%% Armstrong. Hard coded data removed and and futher generalised by 
%% Mike Williams.
%%
%% This module contains the LIM driver process which is the controlles the 
%% Erlang interface to the MD110 LIM hardware. 
%% 
%% Messages to an from the LIM are sent and receved by the lim driver process
%% via an Erlang Prort which thes process creates.
%%
%% Each hardware unit is connected
%% to a position (multiple) in the LIM switch which has 512 multiples. The 
%% exception to this is the switch itself (the BSU). The first two bytes of
%% each message to and from the LIM contain:
%%	Type (4 bits), Flags (2 bits), and Multiple (10 bits).
%% packed in that order. See the module driver_lib which contains routines
%% to decode and encode these fields.
%%
%% Each unit is assumed to be controlled by a single Erlang process.
%% The lim driver process records which process is controllin a unit and
%% which module is to be used to decode and encode the byte (see interface
%% routines subscribe and unsubscribe below). Messages from units for which
%% is no recorded controller process are simply junked (with an error
%% message. Atempt by processes other than the recorded controller process
%% for a unit to send messages to a unit result in that unit being sent an exit
%% message. The lim driver links the recorded controller processes for a unit.
%% If this process should die, the lim driver process (which traps exits)
%% erases the all recorded information about wich units this process is
%% controlling.
%%
%% For each message sent to the LIM, the lim driver creates the list of bytes
%% to be sent to the LIM from the symbolic message by applying the
%% routine 'encode' in the module recorded for the unit. The BSU is
%% special cased so the module driver_bsu is always used.
%%
%% Similarly the the list of bytes received ffrom the LIM is transformed into
%% a symbolic message by applying the rountine 'decode' in the same module 
%% as above.
%%
%% In this way new types of hardware can be added to an existing system
%% without changing this software. The applies mentioned above are encapsulated
%% withing a catch so that this lim driver will not crash if these decode
%% and encode routines should fail.
%%
%% Messages from hardware can be received as
%%
%% 	{lim_driver, {Multiple, Message}}
%%
%% Messages to hardware can be send as
%%
%%     lim_driver ! {self(), msg, {Multiple, Message}}.
%%
%% or by using the function provided
%%
%% 	lim_driver:send_cmd(Multiple, Message)
%%

-module(lim_driver).

-export([start/1,driver/2,subscribe/2,unsubscribe/1,send_cmd/2]).

%% you may need to change this if you move the system
%% this command sytarts the lim driver

%%**********************************************************************
%% The Interface 
%%______________________________________________________________________

%% subscribe/2
%% Tell the lim driver process which driver module to use to encode and decode
%% messages to and from the LIM and to which process is to handle messages to
%% and from a certain multiple in the LIM.

subscribe(Mult,Type) ->
    lim_driver ! {self(), subscribe, Mult, Type},
    receive 
	{lim_driver, subscribed } ->
	    true
    end.

%%______________________________________________________________________

%% unsubscribe/1
%% Tell the lim driver process that the process controlling a multiple is no
%% longer interested in that multiple.

unsubscribe(Mult) ->
    lim_driver ! {self(), unsubscribe, Mult}.

%%______________________________________________________________________

%% send_cmd/2
%% Send a message to the LIM

send_cmd(Mult,Cmd) ->
    lim_driver ! {self(), msg, {Mult, Cmd}}.

%%______________________________________________________________________

%% start/0
%% Start the lim driver process and register it

start(Type) ->
    Id = spawn_link(lim_driver,driver,[self(), Type]),
    register(lim_driver, Id),
    receive
	hw_started ->
	    true
    end.

%%**********************************************************************
%% Routine to start the lim driver process. Called by start/0 above
%% Opens the port controlling the hardware and asocsiated a UNIX
%% process with this port.

driver(Starter, Type) ->
    process_flag(trap_exit, true),
    case Type of
	sim ->
	    Port = none;
	both ->
	    Port = open_port({spawn, configure:limdev()}, [{packet, 2}]),
	    restart_hw(Port,0);
	hw ->
	    Port = open_port({spawn, configure:limdev()}, [{packet, 2}]),
	    restart_hw(Port,0)
    end,
    Starter ! hw_started,			% syncronise with the "starter"
    io:format('Hw started ~n',[]),
    top(Port,empty_tree(),empty, Type).

%%**********************************************************************
%% Local Functions
%%_____________________________________________________________________
%%

%% Restarting HW is so LIM specific that there is no point in not
%% specially codeing it.


restart_hw(Port, 10) ->				% allow max 10 attempts
	io:format('hw restart failed - giving up~n',[]),
	exit('no hardware');

restart_hw(Port, N) ->
	Port ! {self(), {command, [255,255,0]}}, % magic hw reset sequence
	receive 
	    {Port,  {data, [255,255,0]}} ->	% magic reply
	       true
	after 8000 ->	% give it 8 seconds
	    io:format('switch restart failed - retrying~n',[]),
	    restart_hw(Port, N + 1)
	end.

	     
%______________________________________________________________________
%% top loop
%%   NOTE the order of the first Two Messages ---
%%   since the receive loop scans from the top we want to finish old
%%   work before accepting new

%% Port = port for comunication with LIM
%% DevInfoTree = tree containig owners and type of a multiple
%% Bsu0 = Owner of BSU
top(Port,DevInfoTree,Bsu0, Type) ->
	receive
	    {Id, msg,{Mult,Msg}} ->
		send_hw_message(Id, Mult, Msg, DevInfoTree, Port, Bsu0, Type),
		top(Port, DevInfoTree, Bsu0, Type);
	    {Port, {data,Bytes}} ->
		receive_hw_message(Bytes, DevInfoTree, Bsu0, Type),
		top(Port,DevInfoTree,Bsu0, Type);
	    {simulator, {Mult, Msg}} ->
		receive_sim_message(Mult, Msg, DevInfoTree, Bsu0, Type),
		top(Port,DevInfoTree,Bsu0, Type);
	    {Id, subscribe, Mult, Driver} ->
		case check_multiple(Mult) of
		    ok ->
			New_Tree =
			    set_multiple_data(Mult, DevInfoTree, {Id, Driver}),
			link(Id),
			Id ! {lim_driver, subscribed},
			top(Port,New_Tree,Bsu0, Type);
		    bsu ->
			Id ! {lim_driver, subscribed},
			top(Port,DevInfoTree, Id, Type);
		    _ ->
			exit(Id, bad_multiple),
			top(Port, DevInfoTree, Bsu0, Type)
	        end;
	    {Id, unsubscribe, Mult} ->
		case check_multiple(Mult) of
		    ok ->
			New_Tree = set_multiple_data(Mult, DevInfoTree, empty),
			top(Port,New_Tree,Bsu0, Type);
		    bsu ->
			top(Port,DevInfoTree,empty, Type);
		    _ ->
			exit(Id, bad_multiple),
			top(Port,DevInfoTree,Bsu0, Type)
	        end;
	    %% deal with the case that the port crashes,
	    %% nothing we can do but die....
	    {'EXIT', Port, What} ->              
                exit({port_exit,What});
	    %% A linked proces has exited, delete all references to this
	    %% process
	    {'EXIT',_,system_exit} ->
		io:format('LIM Driver terminating~n', []),
	    exit(normal);
	    {'EXIT', Id,  Reason} ->
		io:format('lim_driver got exit message ~w from ~w~n',
			  [Reason, Id]),
		if
		    Id == Bsu0 ->
			Bsu1 = empty;
		    true ->
			Bsu1 = Bsu0
		end,
		top(Port,delete_id(Id,DevInfoTree),Bsu1, Type)
        end.

%%______________________________________________________________________

%% Send a message to the hardware via the port

send_hw_message(Id, Mult, Msg, DevInfoTree, Port, Bsu, Type) ->
    case check_multiple(Mult) of
	ok ->
	    case multiple_data(Mult, DevInfoTree) of
		{Id, Driver} ->
		    case catch apply(Driver, encode, [Msg]) of
			{'ERROR', What} ->
			    exit(Id,{hw_message_error, What, Msg});
			Bytes ->
			    case Type of
				hw ->
				    Port ! {self(),{command,
				       driver_lib:encode(0,0,Mult,Bytes)}};
				both ->
				    Port !{self(),{command,
				       driver_lib:encode(0,0,Mult,Bytes)}},
				    simulator ! {to_hw, Mult, Msg};
				sim ->
				    simulator ! {to_hw, Mult, Msg}
			     end
		     end;
		_ ->				% Sending process is 
						% not the recorded owner
		    exit(Id, unsubscribed_multiple)
	    end;
	%% special case the bsu
	bsu when Bsu /= empty ->
	    case catch apply(driver_bsu, encode, [Msg]) of
		{'ERROR', What} ->
		    exit(Id, {hw_message_error, What, Msg});
		Bytes ->
		   case Type of
		       hw ->
			   Port ! {self(), {command, Bytes}};
		       both ->
			   Port ! {self(), {command, Bytes}},
			   simulator ! {to_hw, Mult, Msg};
		       sim ->
			   simulator ! {to_hw, Mult, Msg}
		   end
	    end;
	_ ->					% send an exit to the 
						% offending process
	    exit(Id, bad_multiple)
     end.


%%______________________________________________________________________

%% Receive a message from hardware

receive_hw_message(Bytes, DevInfoTree, Bsu, Type) ->
    Mult = driver_lib:unit(Bytes),
    case check_multiple(Mult) of
	ok ->
	    case multiple_data(Mult, DevInfoTree) of
		{Id, Driver} ->
		    [_,_|Relevant] = Bytes,	%Skip the mutiple & flags
		    case catch apply(Driver, decode, [Relevant]) of
			{'ERROR', What} ->
			    io:format('bad hw msg ~w ~w ~w~n',
				      [Mult, What, Bytes]);
			Msg ->
			    case Type of
				hw ->
				    true;
				_ ->
				    simulator ! {from_hw, Mult, Msg}
			    end,
			    Id ! {lim_driver, {Mult, Msg}}
		    end;
		_ ->
		    io:format('Unsubscribed message ~w ~w~n', [Mult, Bytes])
	     end;
	bsu when Bsu /= empty ->
	    case catch apply(driver_bsu, decode, [Bytes]) of
		{'ERROR', What} ->
		    io:format('bad hw msg from bsu ~w~n', [Bytes]);
		Msg ->
		    case Type of
			hw -> 
			    true;
			_ ->
			    simulator ! {from_hw, Mult, Msg}
		    end,
		    Bsu ! {lim_driver, Msg}
	    end;
	_ ->
	    io:format('Message to bad multiple ~w ~w~n', [Mult, Bytes])
    end.

%%----------------------------------------------------------------------
% receive a message from the simulator
% Messages are ignored except when the hw is not used

receive_sim_message(Mult, Msg, DevInfoTree, Bsu, sim) ->
    case check_multiple(Mult) of
	ok ->
	    case multiple_data(Mult, DevInfoTree) of
		{Id, Driver} ->
		    Id ! {lim_driver, {Mult, Msg}};
		_ ->
		    io:format('Unsubscribed simulator message ~w ~w~n',
			      [Mult, Msg])
	     end;
	bsu when Bsu /= empty ->
	    Bsu ! {lim_driver, Msg};
	_ ->
	    io:format('Message to bad multiple ~w ~w~n', [Mult, Msg])
    end;

receive_sim_message(Mult, Msg, DevInfoTree, Bsu, _) ->
    io:format("Message ~w to ~w from simulator ignored~n", [Msg, Mult]).


%%______________________________________________________________________
%%
%% The following makes the the assumption that the LIM switch
%% BSU + SSU has 512 multiple positions.
%%
%% Data for these multiple is kept in two layer tree with 32 positions at the
%% top and 16 at the leaves.
%%
%%
%%______________________________________________________________________

% Bounds check and check for bsu
check_multiple(Mult) when Mult >= 0, Mult =< 512 -> ok;
check_multiple(bsu) -> bsu;
check_multiple(_) -> not_ok.
%%______________________________________________________________________

%% routines for creating the tree structure which records which
%% processes control a  unit and what module is to be use to
%% encode/decode messages

empty_tree() ->
    {empty, empty, empty, empty, empty, empty, empty, empty,
     empty, empty, empty, empty, empty, empty, empty, empty,
     empty, empty, empty, empty, empty, empty, empty, empty,
     empty, empty, empty, empty, empty, empty, empty, empty}.

empty_leaf() ->
    {empty, empty, empty, empty, empty, empty, empty, empty,
     empty, empty, empty, empty, empty, empty, empty, empty}.

%%______________________________________________________________________

%% Get the for a multiple from the tree (or say it is the bsu)

multiple_data(bsu,_) ->
    bsu;

multiple_data(Mult,Tree) ->
    case element((Mult div 16) + 1, Tree) of 
	empty ->
	    empty;
	Leaf ->
	    element((Mult rem 16) + 1, Leaf)
    end.

%%______________________________________________________________________

%% set data in the tree for a multiple

set_multiple_data(Mult,Tree, Data)  ->
    Leaf = case element((Mult div 16) + 1, Tree) of 
	empty ->
	    empty_leaf();
	OldLeaf ->
	    OldLeaf
    end,
    NewLeaf = setelement((Mult rem 16) + 1, Leaf,Data),
    setelement((Mult div 16) + 1, Tree, NewLeaf).

%%______________________________________________________________________

%% Routines which scans the entire tree to delete all occurences
%% of a process which has exited (this is assumed to be a rare
%% occurrence

%% Scan the 32 possible part trees
delete_id(Id, Tree) ->
    delete_id(Id, 1, Tree).

delete_id(Id, 33, Tree) -> Tree;
delete_id(Id, N, Tree) ->
    case element(N, Tree) of
	empty ->
	    NewLeaf = empty;
	Leaf ->
	    NewLeaf = delete_id_leaf(Id, 1, Leaf)
    end,
    delete_id(Id, N + 1, setelement(N, Tree, NewLeaf)).

%% Scan the 16 leaves

delete_id_leaf(Id, 17, Leaf) -> Leaf;
delete_id_leaf(Id, N, Leaf) ->
    case element(N, Leaf) of
	{Id, _} ->
	    New = empty;
	New ->
	    New
    end,
    delete_id_leaf(Id, N + 1, setelement(N, Leaf, New)).

%%______________________________________________________________________
