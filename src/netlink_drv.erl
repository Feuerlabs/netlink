%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    launch netlink_drv
%%% @end
%%% Created : 30 Nov 2011 by Tony Rogvall <tony@rogvall.se>

-module(netlink_drv).
-export([open/1, close/1]).
-export([send/2]).
-export([add_membership/2, drop_membership/2,
	 deactivate/1, activate/1, activate/2,
	 debug/2]).

%% deugging
-compile(export_all).

-include("netlink.hrl").

-define(CMD_ADD_MEMBERSHIP,   1).
-define(CMD_DROP_MEMBERSHIP,  2).
-define(CMD_ACTIVE,           3).
-define(CMD_DEBUG,            4).

-define(DLOG_DEBUG,     7).
-define(DLOG_INFO,      6).
-define(DLOG_NOTICE,    5).
-define(DLOG_WARNING,   4).
-define(DLOG_ERROR,     3).
-define(DLOG_CRITICAL,  2).
-define(DLOG_ALERT,     1).
-define(DLOG_EMERGENCY, 0).
-define(DLOG_NONE,     -1).


add_membership(Port,Msg) when is_integer(Msg) ->
    port_call(Port, ?CMD_ADD_MEMBERSHIP, <<Msg:32>>).

drop_membership(Port,Msg) when is_integer(Msg) ->
    port_call(Port, ?CMD_DROP_MEMBERSHIP, <<Msg:32>>).

deactivate(Port) ->
    activate(Port, 0).    

activate(Port) ->
    activate(Port, -1).

activate(Port, N) when is_integer(N), N >= -1, N < 16#7fffffff ->
    port_call(Port, ?CMD_ACTIVE, <<N:32>>).

debug(Port,Level) when is_atom(Level) ->
    L = level(Level),
    port_call(Port, ?CMD_DEBUG, <<L:32>>).


open(Protocol) when is_integer(Protocol), Protocol >= 0 ->
    Driver = "netlink_drv",
    Path = code:priv_dir(netlink),
    io:format("load_driver '~s' from: '~s'\n", [Driver, Path]),
    case erl_ddll:load_driver(Path, Driver) of
	ok ->
	    Arg = integer_to_list(Protocol),
	    erlang:open_port({spawn_driver, Driver++" "++Arg}, [binary]);
	{error,Error} ->
	    io:format("Error: ~s\n", [erl_ddll:format_error_int(Error)]),
	    erlang:error(Error)
    end.

close(Port) ->
    erlang:port_close(Port).

send(Port, Command) ->
    erlang:port_command(Port, Command).

port_call(Port, Cmd, Data) ->
    case erlang:port_control(Port, Cmd, Data) of
	<<0>> ->
	    ok;
	<<255,E/binary>> -> 
	    {error, erlang:binary_to_atom(E, latin1)};
	<<254,E/binary>> -> 
	    {error, binary_to_list(E)};
	<<1,Y>> -> {ok,Y};
	<<2,Y:16/native-unsigned>> -> {ok, Y};
	<<4,Y:32/native-unsigned>> -> {ok, Y};
	<<8,A:32/native-unsigned,B:32/native-unsigned>> -> {ok,A,B};
	<<3,Return/binary>> -> {ok,Return}
    end.    
	
%% convert symbolic to numeric level
level(debug) -> ?DLOG_DEBUG;
level(info)  -> ?DLOG_INFO;
level(notice) -> ?DLOG_NOTICE;
level(warning) -> ?DLOG_WARNING;
level(error) -> ?DLOG_ERROR;
level(critical) -> ?DLOG_CRITICAL;
level(alert) -> ?DLOG_ALERT;
level(emergency) -> ?DLOG_EMERGENCY;
level(none) -> ?DLOG_NONE.

