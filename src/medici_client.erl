%%%-------------------------------------------------------------------
%%% File    : medici.erl
%%% Author  : Jim McCoy <>
%%% Description : principe connection handler and server
%%%
%%% Created :  1 May 2009 by Jim McCoy <>
%%%-------------------------------------------------------------------
-module(medici_client).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(DEFAULT_CONTROLLER, ttserver).
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
    Sock = principe:connect(ClientProps),
%    timer:sleep(50),       % minor wait to let ttserver catch up to connect req
    case get_db_type(Sock) of
	{ok, table} ->
	    DbType = principe_table;
	{ok, _} ->
	    DbType = principe;
	{error, _} ->
	    DbType = nil,   % eliminate a spurious compiler warning...
	    {stop, connect_failure}
    end,
    Controller = proplists:get_value(controller, ClientProps, ?DEFAULT_CONTROLLER),
    Controller ! {client_start, self()},
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
    {stop, {unknown_call, Request}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
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
    io:format("Received info call: ~w~n", [Info]),
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
get_db_type(Socket) ->
    StatInfo = principe:stat(Socket),
    case StatInfo of
	{error, Reason} ->
	    {error, Reason};
	StatList ->
	    DbType = proplists:get_value(type, StatList),
	    case DbType of
		"on-memory hash" -> {ok, hash};
		"table" -> {ok, table};
		"on-memory tree" -> {ok, tree};
		"B+ tree" -> {ok, tree};
		"hash" -> {ok, hash};
		"fixed-length" -> {ok, fixed};
		_ -> {ok, hash}
	    end
    end.
