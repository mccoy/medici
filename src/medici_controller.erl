%%%-------------------------------------------------------------------
%%% File    : medici_controller.erl
%%% Author  : Jim McCoy <>
%%% Description : 
%%%
%%% Created :  5 May 2009 by Jim McCoy <>
%%%-------------------------------------------------------------------
-module(medici_controller).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("medici.hrl").

-record(state, {clients=[], auto_sync=nil, auto_tune=nil}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    {ok, MediciOpts} = application:get_env(options),
    MyName = proplists:get_value(controller_name, MediciOpts, ?CONTROLLER_NAME),
    gen_server:start_link({local, MyName}, ?MODULE, MediciOpts, []).

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
init(_ClientProps) ->
    timer:start(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) when length(State#state.clients) =:= 0 ->
    ?DEBUG_LOG("Request received by controller but no clients available~n", []),
    {reply, {error, no_connection_to_server}, State};
handle_call(start_auto_sync, _From, State) ->
    {reply, ok, start_auto_sync(State, ?DEFAULT_TASK_PERIOD)};
handle_call({start_auto_sync, Period}, _From, State) when is_integer(Period), Period > 0 ->
    {reply, ok, start_auto_sync(State, Period * 1000)};
handle_call(stop_auto_sync, _From, State) ->
    {reply, ok, stop_auto_sync(State)};
handle_call(start_auto_tune, _From, State) ->
    {reply, ok, start_auto_tune(State, ?DEFAULT_TASK_PERIOD)};
handle_call({start_auto_tune, Period}, _From, State) when is_integer(Period), Period > 0 ->
    {reply, ok, start_auto_tune(State, Period * 1000)};
handle_call(stop_auto_tune, _From, State) ->
    {reply, ok, stop_auto_tune(State)};
handle_call({CallFunc}, From, State) ->
    dispatch_request({From, CallFunc}, State);
handle_call({CallFunc, Arg1}, From, State) ->
    dispatch_request({From, CallFunc, Arg1}, State);
handle_call({CallFunc, Arg1, Arg2}, From, State) ->
    dispatch_request({From, CallFunc, Arg1, Arg2}, State);
handle_call({CallFunc, Arg1, Arg2, Arg3}, From, State) ->
    dispatch_request({From, CallFunc, Arg1, Arg2, Arg3}, State);
handle_call({CallFunc, Arg1, Arg2, Arg3, Arg4}, From, State) ->
    dispatch_request({From, CallFunc, Arg1, Arg2, Arg3, Arg4}, State);
handle_call(Request, _From, State) ->
    ?DEBUG_LOG("Unknown request received by controller: ~p", [Request]),
    {reply, {error, invalid_request}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?DEBUG_LOG("Unknown cast received by medici controller: ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({client_start, Pid}, State) ->
    {noreply, State#state{clients=[Pid | State#state.clients]}};

handle_info({client_end, Pid}, State) ->
    {noreply, State#state{clients=lists:delete(Pid, State#state.clients)}};

%% The retries should either do a hard-kill of the clients it is eliminating
%% from the list, but 
handle_info({retry, Pid, _OldReply, OldRequest}, State) when length(State#state.clients) > 1 ->
    dispatch_request(OldRequest, State#state{clients=lists:delete(Pid, State#state.clients)});

handle_info({retry, Pid, OldReply, OldRequest}, State) ->
    gen_server:reply(element(1, OldRequest), OldReply),
    {noreply, State#state{clients=lists:delete(Pid, State#state.clients)}}.


%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
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
dispatch_request(Request, State) when length(State#state.clients) =:= 1 ->
    gen_server:cast(hd(State#state.clients), Request),
    {noreply, State};
dispatch_request(Request, State) ->
    [TgtClient | OtherClients] = State#state.clients,
    gen_server:cast(TgtClient, Request),
    {noreply, State#state{clients=OtherClients++[TgtClient]}}.

start_auto_sync(State, Period) when State#state.auto_sync =:= nil ->
    TRef = timer:send_interval(Period, sync),
    State#state{auto_sync={TRef, Period}};
start_auto_sync(State, Period) ->
    {OldTRef, OldPeriod} = State#state.auto_sync,
    case OldPeriod =/= Period of
	true ->
	    {ok, cancel} = timer:cancel(OldTRef),
	    TRef = timer:send_interval(Period, sync);
	_ ->
	    TRef = OldTRef
    end,
    State#state{auto_sync={TRef, Period}).

stop_auto_sync(State) when State#state.auto_sync =:= nil ->
    State;
stop_auto_sync(State) ->
    {OldTRef, _} = State#state.auto_sync,
    {ok, cancel} = timer:cancel(TRef),
    State#state{auto_sync=nil}.

start_auto_tune(State, Period) when State#state.auto_tune =:= nil ->
    TRef = timer:send_interval(Period, sync),
    State#state{auto_tune={TRef, Period}};
start_auto_tune(State, Period) ->
    {OldTRef, OldPeriod} = State#state.auto_tune,
    case OldPeriod =/= Period of
	true ->
	    {ok, cancel} = timer:cancel(OldTRef),
	    TRef = timer:send_interval(Period, tune);
	_ ->
	    TRef = OldTRef
    end,
    State#state{auto_tune={TRef, Period}).

stop_auto_tune(State) when State#state.auto_tune =:= nil ->
    State;
stop_auto_tune(State) ->
    {OldTRef, _} = State#state.auto_tune,
    {ok, cancel} = timer:cancel(TRef),
    State#state{auto_tune=nil}.
