%%%-------------------------------------------------------------------
%%% File    : messages.hrl
%%% Author  : Ulf Wiger <etxuwig@wsb221>
%%% Description : 
%%%
%%% Created : 15 Oct 2004 by Ulf Wiger <etxuwig@wsb221>
%%%-------------------------------------------------------------------

-record(recv, {lim = [],         % replies, digit, onhook, offhook,...
	       anal = [],        % analysis_reply
	       half_call = []    % request_connection, connect
	      }).

-define(lim, #recv.lim).
-define(anal, #recv.anal).
-define(hc, #recv.half_call).
