%%% @author tony <tony@rogvall.se>
%%% @copyright (C) 2013, tony
%%% @doc
%%%    Get network statistics
%%% @end
%%% Created : 18 Aug 2013 by tony <tony@rogvall.se>

-module(netlink_stat).

-compile(export_all).

-include("../include/netlink.hrl").

get_value(Interface) ->
    netlink:start(),
    {ok,Ref} = netlink:subscribe(Interface),
    netlink:invalidate(Interface, [stats,stats64]),
    ok = netlink:get_match(link, inet, [{stats,native,[]}]),
    Res = get_stats64(Ref,1000),
    flush_stats(Ref),
    netlink:unsubscribe(Ref),
    case Res of
	{ok,Stats} ->
	    #rtnl_link_stats{} = R =list_to_tuple([rtnl_link_stats | Stats]),
	    {ok,R};
	Error ->
	    Error
    end.

get_stats64(Ref,Timeout) ->
    receive
	{netlink,Ref,_Interface,stats64,_Old,New} ->
	    {ok,New}
    after Timeout ->
	    {error,timeout}
    end.

flush_stats(Ref) ->
    receive
	_Msg={netlink,Ref,_Interface,_,_Old,_New} ->
	    %% io:format("flushed: ~p\n", [_Msg]),
	    flush_stats(Ref)
    after 0 ->
	    ok
    end.
