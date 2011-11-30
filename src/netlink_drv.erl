%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2011, Tony Rogvall
%%% @doc
%%%    launch netlink_drv
%%% @end
%%% Created : 30 Nov 2011 by Tony Rogvall <tony@rogvall.se>

-module(netlink_drv).
-export([start/0]).
-export([connect/1, disconnect/1, refresh/1]).
-export([activate/1, activate/2, deactivate/1]).

-define(NL_CMD_CONNECT,     1).
-define(NL_CMD_DISCONNECT,  2).
-define(NL_CMD_ACTIVATE,    3).
-define(NL_CMD_REFRESH,     4).

-define(NL_REP_OK,     0).
-define(NL_REP_ERROR,  1).

start() ->
    Port = open(),
    connect(Port),
    activate(Port),
    refresh(Port),
    Port.

connect(Port) ->
    do_reply(erlang:port_control(Port, ?NL_CMD_CONNECT, [])).

disconnect(Port) ->
    do_reply(erlang:port_control(Port, ?NL_CMD_DISCONNECT, [])).    

deactivate(Port) ->
    activate(Port, 0).    

activate(Port) ->
    activate(Port, -1).

activate(Port, N) when is_integer(N), N >= -1, N < 16#ffff ->
    do_reply(erlang:port_control(Port, ?NL_CMD_ACTIVATE, <<N:16>>)).

refresh(Port) ->
    do_reply(erlang:port_control(Port, ?NL_CMD_REFRESH, [])).

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

do_reply([?NL_REP_OK]) ->
    ok;
do_reply([?NL_REP_ERROR | Err]) ->
    {error, list_to_atom(Err)}.

    

	
