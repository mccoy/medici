%%%-------------------------------------------------------------------
%%% File    : medici_task_mgr.erl
%%% Author  : Jim McCoy <>
%%% Description : 
%%%
%%% Created :  5 May 2009 by Jim McCoy <>
%%%-------------------------------------------------------------------
-module(medici_task_mgr).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("medici.hrl").

%% controller = registered name of the controller
%% port_server = registered name of the port server (if any)
%% tick = frequency of checks for tasks to run (gcd of all task periods)
%% tasks = list of task records
-record(state, {controller, port_server, tasks}).
-record(task, {ref, name, period, module, func, args}).

%%%%%%%%%%%
%%
%% Current Notes:
%%
%% see http://forum.trapexit.org/mailinglists/viewtopic.php?p=3539&sid=f111b8fe8ecd1560bb54fa47e726cbfe
%%
%%%%%%%%%%%

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?TASK_MGR_NAME}, ?MODULE, [], []).

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
init(_Args) ->
    case application:get_env(options) of
	{ok, ApplicationOpts} ->
	    MediciOpts = ApplicationOpts;
	_ ->
	    MediciOpts = []
    end,
    Controller = proplists:get_value(controller_name, MediciOpts, ?CONTROLLER_NAME),
    ServerOpts = proplists:get_value(run_server, MediciOpts, []),
    PortServer = proplists:get_value(server_name, ServerOpts, ?PORT_SRV_NAME),
    case proplists:get_value(auto_sync, MediciOpts, ?AUTO_SYNC) of
	true ->
	    SyncFreq = ?AUTO_SYNC_PERIOD;
	false ->
	    SyncFreq = 0;
	OtherSync when is_integer(OtherSync) ->
	    SyncFreq = OtherSync
    end,
    case proplists:get_bool(auto_tune, MediciOpts, ?AUTO_TUNE) of
	true ->
	    TuneFreq = ?AUTO_TUNE_PERIOD;
	_ ->
	    TuneFreq = 0
    end,
    case SyncFreq > ?MIN_PERIOD of
	true ->
	    PostSyncTasks = add_periodic_task(sync, SyncFreq, ?MODULE, auto_sync, []);
	_ ->
	    PostSyncTasks = []
    end,
    case TuneFreq > ?MIN_PERIOD of
	true ->
	    PostTuneTasks = lists:append(PostSyncTasks, add_periodic_task(tune, TuneFreq, ?MODULE, auto_tune, []));
	_ ->
	    PostTuneTasks = PostSyncTasks
    end,
    {ok, #state{controller=Controller,
	        port_server=PortServer,
	        tasks=PostTuneTasks}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add_task, Name, Period, M, F, A}, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    CancelTask = fun(Task) ->
			 timer:cancel(Task#task.ref)
		 end,
    lists:foreach(CancelTask, State#state.tasks),
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
add_periodic_task(Name, Period, Module, Function, Args) ->
    #task{}.


%%%%%%%%%
%  watching disk space
%
%  skip os_mon
%
% windows: 
%% fsutil volume diskfree C:

%% which returns something like:

%% Total # of free bytes : 230645665792
%% Total # of bytes : 266205130752
%% Total # of avail free bytes : 230645665792

% unix/osx
%
% /bin/df -k watchdir
%
%% Filesystem   1024-blocks      Used Available Capacity  Mounted on
%% /dev/disk0s2   116884912 111372820   5256092    96%    /

