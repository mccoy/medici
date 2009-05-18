%%%-------------------------------------------------------------------
%%% File    : medici_conn.erl
%%% Author  : Jim McCoy <>
%%% Description : principe connection handler and server
%%%
%%% Created :  1 May 2009 by Jim McCoy <>
%%%-------------------------------------------------------------------
-module(medici_conn).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(DEFAULT_CONTROLLER, medici).
-ifdef(DEBUG).
-define(DEBUG_LOG(Msg, Args), error_logger:error_msg(Msg, Args)).
-else.
-define(DEBUG_LOG(_Msg, _Args), void).
-endif.

-record(state, {socket, mod, controller}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(ClientProps) ->
    gen_server:start_link(?MODULE, [ClientProps], []).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(ClientProps) ->
    TmpMod = principe:new(bad_val),
    {ok, Sock} = TmpMod:connect(ClientProps),
    case get_db_type(Sock) of
	{ok, Endian, table} ->
	    Principe = principe:new(Endian),
	    DbType = principe_table:new(Principe);
	{ok, big, _} ->
	    DbType = principe:new(big);
	{ok, little, _} ->
	    DbType = principe:new(little);
	{error, _} ->
	    DbType = nil,   % eliminate a spurious compiler warning...
	    {stop, connect_failure}
    end,
    Controller = proplists:get_value(controller, ClientProps, ?DEFAULT_CONTROLLER),
    Controller ! {client_start, self()},
    process_flag(trap_exit, true),
    {ok, #state{socket=Sock, mod=DbType, controller=Controller}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
    ?DEBUG_LOG("Unknown call ~p~n", [Request]),
    {stop, {unknown_call, Request}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, asked_to_stop, State};
handle_cast({From, CallFunc}=Request, State) when is_atom(CallFunc) ->
    Module = State#state.mod,
    Result = Module:CallFunc(State#state.socket),
    case Result of
	{error, conn_closed} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	{error, conn_error} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	_ ->
	    gen_server:reply(From, Result),
	    {noreply, state}
    end;
handle_cast({From, CallFunc, Arg1}=Request, State) when is_atom(CallFunc) ->
    Module = State#state.mod,
    Result = Module:CallFunc(State#state.socket, Arg1),
    case Result of
	{error, conn_closed} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	{error, conn_error} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	_ ->
	    gen_server:reply(From, Result),
	    {noreply, state}
    end;
handle_cast({From, CallFunc, Arg1, Arg2}=Request, State) when is_atom(CallFunc) ->
    Module = State#state.mod,
    Result = Module:CallFunc(State#state.socket, Arg1, Arg2),
    case Result of
	{error, conn_closed} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	{error, conn_error} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	_ ->
	    gen_server:reply(From, Result),
	    {noreply, state}
    end;
handle_cast({From, CallFunc, Arg1, Arg2, Arg3}=Request, State) when is_atom(CallFunc) ->
    Module = State#state.mod,
    Result = Module:CallFunc(State#state.socket, Arg1, Arg2, Arg3),
    case Result of
	{error, conn_closed} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	{error, conn_error} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	_ ->
	    gen_server:reply(From, Result),
	    {noreply, state}
    end;
handle_cast({From, CallFunc, Arg1, Arg2, Arg3, Arg4}=Request, State) when is_atom(CallFunc) ->
    Module = State#state.mod,
    Result = Module:CallFunc(State#state.socket, Arg1, Arg2, Arg3, Arg4),
    case Result of
	{error, conn_closed} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	{error, conn_error} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	_ ->
	    gen_server:reply(From, Result),
	    {noreply, state}
    end.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    ?DEBUG_LOG("An unknown info message was received: ~w~n", [Info]),
    %%% XXX: does this handle tcp connection closed events?
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#state.socket),
    State#state.controller ! {client_end, self()},
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% Query the remote end of the socket to get the remote database type
get_db_type(Socket) when is_port(Socket) ->
    TmpMod = principe:new(bad_val),
    StatInfo = TmpMod:stat(Socket),
    case StatInfo of
	{error, Reason} ->
	    {error, Reason};
	StatList ->
	    case proplists:get_value(bigend, StatList) of
		"0" ->
		    Endian = little;
		_ ->
		    Endian = big
	    end,
	    case proplists:get_value(type, StatList) of
		"on-memory hash" -> 
		    Type = hash;
		"table" -> 
		    Type = table;
		"on-memory tree" -> 
		    Type = tree;
		"B+ tree" -> 
		    Type = tree;
		"hash" ->
		    Type = hash;
		"fixed-length" ->
		    Type = fixed;
		_ -> 
		    ?DEBUG_LOG("~p:get_db_type returned ~p~n", [?MODULE, proplists:get_value(type, StatList)]),
		    Type = error
	    end,
	    case Type of
		error ->
		    {error, unknown_db_type};
		_ ->
		    {ok, Endian, Type}
	    end	    
    end.
