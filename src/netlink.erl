%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
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

-define(SERVER, ?MODULE). 

-record(interface,
	{
	  name,
	  index,
	  mtu,
	  txqlen,
	  flags,
	  if_state,
	  if_lower_state,
	  oper_status,
	  link_mode,
	  addr
	}).

-record(state, 
	{
	  if_list = [] :: #interface {}
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
    {ok, #state{}}.

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
	      case match(I, Match) of
		  true ->
		      io:format("~s { name ~s; index ~w; mtu ~w; txtqlen ~w; "
				"flags ~w; if_state ~w; if_lower_state ~w; "
				"oper_status ~p; link_mode ~p; addr ~p; }\n",
				tuple_to_list(I));
		  false ->
		      ok
	      end
      end, State#state.if_list),
    {reply, ok, State};
handle_call({match,Match}, _From, State) ->
    IfList = lists:filter(fun(I) -> match(I,Match) end, State#state.if_list),
    {reply, IfList, State};
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
handle_info({netlink,As}, State) ->
    case proplists:get_value(name, As, "") of
	"" -> 
	    {noreply,State};
	Name ->
	    case lists:keytake(Name, #interface.name, State#state.if_list) of
		false -> 
		    Ifs = [if_update(#interface{}, As)|State#state.if_list],
		    {noreply, State#state { if_list=Ifs }};
		{value,If,Ifs0} ->
		    Ifs = [if_update(If, As)|Ifs0],
		    {noreply, State#state { if_list=Ifs }}
	    end
    end;
handle_info(_Info, State) ->
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
-define(UPDATE(I,F,V), 
	(I)#interface{F=v_update((I),F,(I)#interface.F, (V))}).

if_update(I, [Kv | Kvs]) ->
    case Kv of
	{name,V} ->
	    if_update(?UPDATE(I,name,V), Kvs);
	{index,V} ->
	    if_update(?UPDATE(I,index,V), Kvs);
	{mtu,V} ->
	    if_update(?UPDATE(I,mtu,V), Kvs);
	{txqlen,V} ->
	    if_update(?UPDATE(I,txqlen,V), Kvs);
	{flags,V} ->
	    if_update(?UPDATE(I,flags,V), Kvs);	
	{if_state,V} ->
	    if_update(?UPDATE(I,if_state,V), Kvs);
	{if_lower_state,V} ->
	    if_update(?UPDATE(I,if_lower_state,V), Kvs);
	{oper_status,V} ->
	    if_update(?UPDATE(I,oper_status,V), Kvs);	
	{link_mode,V} ->
	    if_update(?UPDATE(I,link_mode,V), Kvs);
	{addr,V} ->
	    if_update(?UPDATE(I,addr,V), Kvs);
	_ -> 
	    io:format("unknown interface option: ~p\n", [Kv]),
	    if_update(I, Kvs)
    end;
if_update(I, []) ->
    I.

v_update(_I,_Field,X,X) -> %% no changed
    X;
v_update(_I,name,undefined,New) ->
    io:format("found interface ~s\n", [New]),
    New;
v_update(I,Field,undefined,New) ->
    io:format("~s: ~s = ~p\n", [I#interface.name,Field,New]),
    New;
v_update(I,flags,Old,New) ->
    Added = New -- Old,
    Removed = Old -- New,
    io:format("~s: flags added ~p, removed ~p\n", 
	      [I#interface.name,Added,Removed]),
    New;
v_update(I,Field,Old,New) ->
    io:format("~s: ~s changed from ~p to ~p\n",
	      [I#interface.name,Field,Old,New]),
    New.

match(I, [{Field,Value}|Match]) when is_atom(Field) ->
    try v_get(I,Field) of
	Value -> match(I, Match);
	_ -> false
    catch
	error:_ -> false
    end;
match(I, [{Op,Field,Value}|Match]) when is_atom(Op),is_atom(Field) ->
    try v_get(I,Field) of
	FValue ->
	    case compare(Op,FValue,Value) of
		true ->
		    match(I, Match);
		false ->
		    false
	    end
    catch
	error:_ -> false
    end;
match(_I, []) ->
    true.


v_get(I, name) -> I#interface.name;
v_get(I, index) -> I#interface.index;
v_get(I, mtu) -> I#interface.mtu;
v_get(I, txqlen) -> I#interface.txqlen;
v_get(I, flags) -> I#interface.flags;
v_get(I, if_state) -> I#interface.if_state;
v_get(I, if_lower_state) -> I#interface.if_lower_state;
v_get(I, oper_status) -> I#interface.oper_status;
v_get(I, link_mode) -> I#interface.link_mode;
v_get(I, addr) -> I#interface.addr.

compare('==',A,B) -> A == B;
compare('=:=',A,B) -> A =:= B;
compare('<' ,A,B) -> A < B;
compare('=<' ,A,B) -> A =< B;
compare('>' ,A,B) -> A > B;
compare('>=' ,A,B) -> A >= B;
compare('/=' ,A,B) -> A /= B;
compare('=/=' ,A,B) -> A =/= B;
compare(_,_,_) -> false.
