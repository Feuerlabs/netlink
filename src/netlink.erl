%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2011, Tony Rogvall
%%% @doc
%%%    netlink 
%%% @end
%%% Created : 30 Nov 2011 by Tony Rogvall <tony@rogvall.se>

-module(netlink).
-export([start/0]).

start() ->
    application:start(netlink).
