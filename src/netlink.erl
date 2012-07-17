%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    Netlink state monitor
%%% @end
%%% Created : 11 Jun 2012 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(netlink).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).
-export([i/0, list/1, select/1]).
-export([subscribe/1, subscribe/2, subscribe/3]).
-export([unsubscribe/1]).

-define(SERVER, ?MODULE). 

-type if_inet_field() ::
	{inet,addr} | {inet,bcast} | {inet,mcast} | {inet,peer} | 
	{inet,prefixlen}.

-type if_inet6_field() ::
	{inet6,addr} | {inet6,bcast} | {inet6,mcast} | {inet6,peer} | 
	{inet6,prefixlen}.	

-type if_addr_field() :: if_inet_field() | if_inet6_field().

-type if_link_field() :: name | index | mtu | txqlen | flags | 
			 if_state | if_lower_state | oper_status |
			 qdisc | {eth,addr} | {eth,bcast}.

-type if_field() :: if_link_field() | if_addr_field().

-type if_name() :: string().
-type if_flag() :: atom().

-record(link,
	{
	  name     :: if_name(),
	  index    :: non_neg_integer(),
	  mtu      :: non_neg_integer(),
	  txqlen   :: non_neg_integer(),
	  flags    :: [if_flag()],
	  if_state :: up | down,
	  if_lower_state :: up | down,
	  oper_status :: string(),
	  link_mode   :: string(),
	  qdisc      :: string(),
	  addr,   %% link address
	  bcast   %% link broadcast
	}).

-record(address,
	{
	  addr,   %% local address
	  bcast,  %% broadcast address
	  mcast,  %% multicast address 
	  peer,   %% peer address (p-to-p)
	  prefixlen
	}).

%% name == link_name => main interface!
-record(interface,
	{
	  name       :: if_name(),  %% interface name
	  link_name  :: if_name(),  %% link name
	  index      :: integer(),  %% link index
	  inet       :: #address{},
	  inet6      :: #address{}
	}).

-record(subscription,
	{
	  pid  :: pid(),               %% subscriber
	  mon  :: reference(),         %% monitor
	  name :: string(),            %% name
	  fields=all :: all | [if_field()]
	}).

-record(state, 
	{
	  port,
	  if_list   = [] :: #interface {},
	  link_list = [] :: #link {},
	  sub_list  = [] :: #subscription {}
        }).

%%%===================================================================
%%% API
%%%===================================================================

%% debug & test
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

i() ->
    gen_server:call(?SERVER, {list,[]}).

stop() ->
    gen_server:call(?SERVER, stop).

select(Match) ->
    gen_server:call(?SERVER, {select,Match}).

list(Match) ->
    gen_server:call(?SERVER, {list,Match}).

%% @doc
%%   Subscribe to interface changes, notifications will be
%%   sent in {netlink,reference(),if_name(),if_field(),OldValue,NewValue}
%% @end

-spec subscribe(Name::string()) ->
		       {ok,reference()}.

subscribe(Name) ->
    subscribe(Name,all,[]).

-spec subscribe(Name::string(),Fields::all|[if_field()]) ->
		       {ok,reference()}.

subscribe(Name,Fields) ->
    subscribe(Name,Fields, []).

-spec subscribe(Name::string(),Fields::all|[if_field()],Otions::[atom()]) ->
		       {ok,reference()}.
subscribe(Name,Fields,Options) ->
    gen_server:call(?SERVER, {subscribe, self(),Name,Options,Fields}).


unsubscribe(Ref) ->
    gen_server:call(?SERVER, {unsubscribe,Ref}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Port = netlink_drv:open(),
    netlink_drv:connect(Port),
    netlink_drv:activate(Port),
    netlink_drv:refresh(Port),
    {ok, #state{ port=Port }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({list,Match}, _From, State) ->
    lists:foreach(
      fun(I) ->
	      L = lists:keyfind(I#interface.index, 
				#link.index, State#state.link_list),
	      case match(I,L,Match) of
		  true ->
		      io:format("interface { ~s ~s}\n",
				[format_interface(I),format_link(L)]);
		  false ->
		      ok
	      end
      end, State#state.if_list),
    {reply, ok, State};
handle_call({match,Match}, _From, State) ->
    IfList = lists:filter(
	       fun(I) -> 
		       L = lists:keyfind(I#interface.index, 
					 #link.index, State#state.link_list),
		       match(I,L,Match) 
	       end, 
	       State#state.if_list),
    {reply, IfList, State};
handle_call({subscribe, Pid, Name, Options, Fs}, _From, State) ->
    Mon = erlang:monitor(process, Pid),
    S = #subscription { pid=Pid, mon=Mon, name=Name, fields=Fs },
    SList = [S | State#state.sub_list],
    case proplists:get_bool(flush, Options) of
	false ->
	    {reply, {ok,Mon}, State#state { sub_list = SList }};
	true ->
	    lists:foreach(
	      fun(I) ->
		      Kvs = interface_to_kv(I),
		      if_update(#interface{}, Kvs, [S])
	      end, State#state.if_list),
	    lists:foreach(
	      fun(L) ->
		      Kvs = link_to_kv(L),
		      link_update(#link{}, Kvs, [S])
	      end, State#state.link_list),
	    {reply, {ok,Mon}, State#state { sub_list = SList }}
    end;
handle_call({unsubscribe,Ref}, _From, State) ->
    case lists:keytake(Ref, #subscription.mon, State#state.sub_list) of
	false ->
	    {reply,ok,State};
	{value,_S,SubList} ->
	    erlang:demonitor(Ref),
	    {reply,ok,State#state { sub_list=SubList }}
    end;
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info={netlink,Port,"route/link",As}, State) 
  when Port =:= State#state.port ->
    io:format("handle_info: ~p\n", [_Info]),
    case proplists:get_value(name, As, "") of
	"" -> 
	    {noreply,State};
	Name ->
	    State1 = update_link_by_name(Name, As, State),
	    {noreply, State1}
    end;

handle_info(_Info={netlink,Port,"route/addr",As}, State) 
  when Port =:= State#state.port ->
    io:format("handle_info: ~p\n", [_Info]),
    case proplists:get_value(index, As) of
	undefined ->
	    {noreply,State};	
	Index ->
	    L = lists:keyfind(Index, #link.index, State#state.link_list),
	    if L =:= false ->
		    io:format("No such index ~p\n", [Index]),
		    {noreply,State};
	       true ->
		    Label = proplists:get_value(label,As),
		    As1 = [{link_name,L#link.name}|proplists:delete(label,As)],
		    State1 = update_if(Index, Label, As1, State),
		    {noreply, State1}
	    end
    end;
handle_info({'DOWN',Ref,process,Pid,Reason}, State) ->
    case lists:keytake(Ref, #subscription.mon, State#state.sub_list) of
	false ->
	    {noreply,State};
	{value,_S,SubList} ->
	    io:format("subscription from pid ~p deleted reason=~p\n",
		      [Pid, Reason]),
	    {noreply,State#state { sub_list=SubList }}
    end;
handle_info(_Info, State) ->
    io:format("INFO: ~p\n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% convert iterface to key value list
interface_to_kv(If) when is_record(If,interface) ->
    lists:zipwith(
      fun(K,Vi) -> {K,element(Vi,If)} end, 
      record_info(fields, interface),
      lists:seq(2, tuple_size(If))).

%% convert iterface to key value list
link_to_kv(L) when is_record(L,link) ->
    lists:zipwith(
      fun(K,Vi) -> {K,element(Vi,L)} end, 
      record_info(fields, link),
      lists:seq(2, tuple_size(L))).
    

update_link_by_name(Name, As, State) ->
    case lists:keytake(Name, #link.name, State#state.link_list) of
	false ->
	    Ls = [link_update(#link{}, As, State#state.sub_list)|
		  State#state.link_list],
	    State#state { link_list=Ls };
	{value,Link,Ls0} ->
	    Ls = [link_update(Link, As,State#state.sub_list)|Ls0],
	    State#state { link_list=Ls }
    end.

%% 
%% 
%%
update_if(Index,undefined,As,State) ->
    update_if_by_index(Index, As, State);
update_if(_Index,Label,As,State) ->
    case lists:keytake(Label, #interface.name, State#state.if_list) of
	false ->
	    %% create new "virtual" interface
	    Is = [if_update(#interface{name=Label}, As, State#state.sub_list) |
		  State#state.if_list],
	    State#state { if_list=Is };
	{value,I,Is0} ->
	    %% update "virtual" interface
	    Is = [if_update(I, As, State#state.sub_list)|Is0],
	    State#state { if_list=Is }
    end.

update_if_by_index(Index, As, State) ->
    case lists:keytake(Index, #interface.index, State#state.if_list) of
	false ->
	    Is = [if_update(#interface{}, As,State#state.sub_list) |
		  State#state.if_list],
	    State#state { if_list=Is };
	{value,I,Is0} ->
	    Is = [if_update(I, As, State#state.sub_list)|Is0],
	    State#state { if_list=Is }
    end.


-define(LUPDATE(I,F,V,S), 
	(I)#link{F=l_update((I),F,(I)#link.F, (V),S)}).

link_update(L, [Kv | Kvs], S) ->
    case Kv of
	{name,V} ->
	    link_update(?LUPDATE(L,name,V,S), Kvs, S);
	{index,V} ->
	    link_update(?LUPDATE(L,index,V,S), Kvs, S);
	{mtu,V} ->
	    link_update(?LUPDATE(L,mtu,V,S), Kvs, S);
	{txqlen,V} ->
	    link_update(?LUPDATE(L,txqlen,V,S), Kvs, S);
	{flags,V} ->
	    link_update(?LUPDATE(L,flags,V,S), Kvs, S);	
	{if_state,V} ->
	    link_update(?LUPDATE(L,if_state,V,S), Kvs, S);
	{if_lower_state,V} ->
	    link_update(?LUPDATE(L,if_lower_state,V,S), Kvs, S);
	{oper_status,V} ->
	    link_update(?LUPDATE(L,oper_status,V,S), Kvs, S);	
	{link_mode,V} ->
	    link_update(?LUPDATE(L,link_mode,V,S), Kvs, S);
	{addr,V} ->
	    link_update(?LUPDATE(L,addr,V,S), Kvs, S);
	{bcast,V} ->
	    link_update(?LUPDATE(L,bcast,V,S), Kvs, S);
	{qdisc,V} ->
	    link_update(?LUPDATE(L,qdisc,V,S), Kvs, S);
	_ -> 
	    io:format("unknown link attribute: ~p\n", [Kv]),
	    link_update(L, Kvs,S)
    end;
link_update(L, [], _S) ->
    L.

-define(IUPDATE(I,F,V,S), 
	(I)#interface{F=i_update((I),F,(I)#interface.F, (V),S)}).

if_update(I, [Kv | Kvs], S) ->
    case Kv of
	{name,V} ->
	    if_update(?IUPDATE(I,name,V,S), Kvs, S);
	{link_name,V} ->
	    if_update(?IUPDATE(I,link_name,V,S), Kvs, S);
	{index,V} ->
	    if_update(?IUPDATE(I,index,V,S), Kvs, S);
	{inet,V={addr,_}} ->
	    if_update(?IUPDATE(I,inet,V,S), Kvs, S);
	{inet,V={bcast,_}} ->
	    if_update(?IUPDATE(I,inet,V,S), Kvs, S);
	{inet,V={mcast,_}} ->
	    if_update(?IUPDATE(I,inet,V,S), Kvs, S);
	{inet,V={peer,_}} ->
	    if_update(?IUPDATE(I,inet,V,S), Kvs, S);
	{inet,V={prefixlen,_}} ->
	    if_update(?IUPDATE(I,inet,V,S), Kvs, S);

	{inet6,V={addr,_}} ->
	    if_update(?IUPDATE(I,inet6,V,S), Kvs, S);
	{inet6,V={bcast,_}} ->
	    if_update(?IUPDATE(I,inet6,V,S), Kvs, S);
	{inet6,V={mcast,_}} ->
	    if_update(?IUPDATE(I,inet6,V,S), Kvs, S);
	{inet6,V={peer,_}} ->
	    if_update(?IUPDATE(I,inet6,V,S), Kvs, S);
	{inet6,V={prefixlen,_}} ->
	    if_update(?IUPDATE(I,inet6,V,S), Kvs, S);

	_ -> 
	    io:format("unknown interface option: ~p\n", [Kv]),
	    if_update(I, Kvs,S)
    end;
if_update(I, [], _S) ->
    I.

i_update(_L,_Field,X,X,_S) -> %% not changed
    X;
i_update(I,inet,_Old,{F,V},S) ->
    A = if I#interface.inet =:= undefined -> #address{};
	   true -> I#interface.inet
	end,
    a_update(I, inet, A, F, V, S);
i_update(I,inet6,_Old,{F,V},S) ->
    A = if I#interface.inet6 =:= undefined -> #address{};
	   true -> I#interface.inet6
	end,
    a_update(I, inet6, A, F, V, S);
i_update(_I,index,_Old,New, _S) ->
    New;
i_update(_I,link_name,_Old,New, _S) ->
    New;
i_update(_I,name,_Old,New, _S) ->
    New.

a_update(I, Fam, A, F, V, S) ->
    Name = I#interface.name,
    case F of
	addr  -> 
	    send_event(Name,{Fam,F},A#address.addr,V,S),
	    A#address { addr = V };
	bcast -> 
	    send_event(Name,{Fam,F},A#address.bcast,V,S),
	    A#address { bcast = V };
	mcast -> 
	    send_event(Name,{Fam,F},A#address.mcast,V,S),
	    A#address { mcast = V };
	peer  ->
	    send_event(Name,{Fam,F},A#address.peer,V,S),
	    A#address { peer = V };
	prefixlen ->
	    send_event(Name,{Fam,F},A#address.prefixlen,V,S),
	    A#address { prefixlen = V }
    end.


l_update(_L,_Field,X,X,_S) -> %% not changed
    X;
l_update(_L,name,undefined,New,S) ->
    io:format("found interface ~s\n", [New]),
    send_event(New,name,undefined,New,S),
    New;
l_update(L,Field,undefined,New,S) ->
    io:format("~s: ~s = ~p\n", [L#link.name,Field,New]),
    send_event(L#link.name,Field,undefined,New,S),
    New;
l_update(L,flags,Old,New,S) ->
    Added = New -- Old,
    Removed = Old -- New,
    io:format("~s: flags added ~p, removed ~p\n", 
	      [L#link.name,Added,Removed]),
    send_event(L#link.name,flags,Old,New,S),
    New;
l_update(L,Field,Old,New,S) ->
    io:format("~s: ~s changed from ~p to ~p\n",
	      [L#link.name,Field,Old,New]),
    send_event(L#link.name,Field,Old,New,S),
    New.


send_event(Name,Field,Old,New,[S|SList]) when 
      S#subscription.name =:= Name; S#subscription.name =:= all ->
    case S#subscription.fields=:=all orelse
	lists:member(Field,S#subscription.fields) of
	true ->
	    S#subscription.pid ! {netlink,S#subscription.mon,
				  Name,Field,Old,New},
	    send_event(Name,Field,Old,New,SList);
	false ->
	    send_event(Name,Field,Old,New,SList)
    end;
send_event(Name,Field,Old,New,[_|SList]) ->
    send_event(Name,Field,Old,New,SList);
send_event(_Name,_Field,_Old,_New,[]) ->
    ok.
	    


match(I,L,[{Field,Value}|Match]) when is_atom(Field) ->
    try v_get(I,L,Field) of
	Value -> match(I, L, Match);
	_ -> false
    catch
	error:_ -> false
    end;
match(I,L,[{Op,Field,Value}|Match]) when is_atom(Op),is_atom(Field) ->
    try v_get(I,L,Field) of
	FValue ->
	    case compare(Op,FValue,Value) of
		true ->
		    match(I,L,Match);
		false ->
		    false
	    end
    catch
	error:_ -> false
    end;
match(_I, _L, []) ->
    true.

%% select either link attribute or interface attribute
v_get(I,_L, inet)   -> I#interface.inet;
v_get(I,_L, inet6)  -> I#interface.inet6;
v_get(I,_L,{inet,F}) -> v_addr(I#interface.inet,F);
v_get(I,_L,{inet6,F}) -> v_addr(I#interface.inet6,F);
v_get(I,_L,link_name) -> I#interface.link_name;
v_get(_I,false,_F) -> undefined;
v_get(_I,L, name) -> L#link.name;
v_get(_I,L, index) -> L#link.index;
v_get(_I,L, mtu) -> L#link.mtu;
v_get(_I,L, txqlen) -> L#link.txqlen;
v_get(_I,L, flags) -> L#link.flags;
v_get(_I,L, if_state) -> L#link.if_state;
v_get(_I,L, if_lower_state) -> L#link.if_lower_state;
v_get(_I,L, oper_status) -> L#link.oper_status;
v_get(_I,L, link_mode) -> L#link.link_mode;
v_get(_I,L, qdisc) -> L#link.qdisc;
v_get(I, _L, F) -> v_addr(I#interface.inet,F).



v_addr(undefined, _) -> undefined;
v_addr(A, addr)  -> A#address.addr;
v_addr(A, bcast) -> A#address.bcast;
v_addr(A, mcast) -> A#address.mcast;
v_addr(A, peer)  -> A#address.peer;
v_addr(A, prefixlen) -> A#address.prefixlen.


format_link(L) ->
    lists:zipwith(
      fun(K,Vi) ->
	      [atom_to_list(K), " ",l_format(K, element(Vi,L)),";"]
      end,
      record_info(fields, link),
      lists:seq(2, tuple_size(L))).

format_interface(I) ->
    lists:zipwith(
      fun(K,Vi) ->
	      [atom_to_list(K), " ",i_format(K, element(Vi,I)),";"]
      end,
      record_info(fields, interface),
      lists:seq(2, tuple_size(I))).

format_address(_F,undefined) ->
    "";
format_address(F,A) ->
    lists:zipwith(
      fun(K,Vi) ->
	      [atom_to_list(K), " ",a_format(F,K,element(Vi,A)),";"]
      end,
      record_info(fields, address),
      lists:seq(2, tuple_size(A))).
    

l_format(name, V) -> io_lib:format("~s", [V]);
l_format(oper_status,V) -> io_lib:format("~s", [V]);
l_format(link_mode,V) -> io_lib:format("~s", [V]);
l_format(qdisc,V) -> io_lib:format("~s", [V]);
l_format(addr,V) ->  format_addr(eth, V);
l_format(bcast,V) -> format_addr(eth, V);
l_format(_, V) -> io_lib:format("~w", [V]).

i_format(name, V)       -> io_lib:format("~s", [V]);
i_format(link_name, V)  -> io_lib:format("~s", [V]);
i_format(index, V)      -> io_lib:format("~w", [V]);
i_format(inet, V)       -> format_address(inet,V);
i_format(inet6, V)      -> format_address(inet6,V);
i_format(_, V) -> io_lib:format("~w", [V]).
    
a_format(F,addr,V)  -> format_addr(F, {addr,V});
a_format(F,bcast,V) -> format_addr(F, {bcast,V});
a_format(F,mcast,V) -> format_addr(F, {mcast,V});
a_format(F,peer,V) -> format_addr(F, {peer,V});
a_format(_F,prefixlen,V) -> io_lib:format("~w", [V]);
a_format(_F,_K,V) -> io_lib:format("~w", [V]).
    

format_addr(_, undefined) -> "";
format_addr(eth, Addr) when is_tuple(Addr), tuple_size(Addr) =:= 6 ->
    io_lib:format("~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:",
		  tuple_to_list(Addr));
format_addr(_Fam, {_,undefined}) -> "";
format_addr(_Fam, {addr,A}) ->
    ["addr ", inet_parse:ntoa(A)];
format_addr(_Fam, {bcast,A}) ->
    ["bcast ", inet_parse:ntoa(A)];
format_addr(_Fam, {mcast,A}) ->
    ["mcast ", inet_parse:ntoa(A)];
format_addr(_Fam, {peer,A}) ->
    ["peer ", inet_parse:ntoa(A)].


    
compare('==',A,B) -> A == B;
compare('=:=',A,B) -> A =:= B;
compare('<' ,A,B) -> A < B;
compare('=<' ,A,B) -> A =< B;
compare('>' ,A,B) -> A > B;
compare('>=' ,A,B) -> A >= B;
compare('/=' ,A,B) -> A /= B;
compare('=/=' ,A,B) -> A =/= B;
compare(_,_,_) -> false.
