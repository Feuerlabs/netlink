%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2011, Tony Rogvall
%%% @doc
%%%    launch netlink_drv
%%% @end
%%% Created : 30 Nov 2011 by Tony Rogvall <tony@rogvall.se>

-module(netlink_drv).
-export([start/0]).

-define(NL_CONNECT,     1).
-define(NL_DISCONNECT,  2).
-define(NL_ACTIVATE,    3).
-define(NL_DEACTIVATE,  4).
-define(NL_ACTIVATE_1,  5).

start() ->
    Port = open(),
    erlang:port_control(Port, ?NL_CONNECT, []),
    erlang:port_control(Port, ?NL_ACTIVATE, []),
    Port.

open() ->
    Driver = "netlink_drv",
    Path = code:priv_dir(netlink),
    io:format("load_driver '~s' from: '~s'\n", [Driver, Path]),
    case erl_ddll:load_driver(Path, Driver) of
	ok ->
	    erlang:open_port({spawn_driver, Driver}, [binary]);
	{error,Error} ->
	    io:format("Error: ~s\n", [erl_ddll:format_error_int(Error)]),
	    erlang:error(Error)
    end.


	
