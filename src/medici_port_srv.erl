%%%-------------------------------------------------------------------
%%% File    : medici_port_srv.erl
%%% Author  : Jim McCoy <>
%%% Description : 
%%%
%%% Created : 30 May 2009 by Jim McCoy <>
%%%-------------------------------------------------------------------
-module(medici_port_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% The options part of the state record is a collection of proplists
%% for the various tokyo tyrant options. There are five key/string proplists
%% in the tyrant_command list:
%%   server_bin - single element, the path to the tyrant binary
%%   data_file - single element, the path to the database file
%%   tyrant_opts - proplist, key/value list of command-line tyrant flags
%%   tuning_opts - proplist, key/value list of tyrant tuning options (appended
%%                 to data_file name when invoking tyrant command)
%%   port_opts - list of options to be used by open_port
-record(state, {port=nil, 
		options=[],
		pid=0,
	        log_match,
	        pid_match}).

-define(PORT_OPTS, [binary, use_stdio, stream, {line, 256}, hide]).
-define(TYRANT_BIN, "/opt/local/bin/ttserver").
-define(TYRANT_OPTS, []).
-define(DATA_FILE, "\"*\""). % default to in-memory hash (quote the *...)
-define(TUNING_OPTS, []).
-define(LOG_REGEXP, "\\S+\\t(\\S+)").
-define(PID_REGEXP, "service started: (\\d+)").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
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
init([]) ->
    {ok, LogMatch} = re:compile(?LOG_REGEXP),
    {ok, PidMatch} = re:compile(?PID_REGEXP),
    case application:get_env(options) of
	{ok, MediciOpts} ->
	    process_flag(trap_exit, true),
	    start_server(MediciOpts, #state{log_match=LogMatch,
					    pid_match=PidMatch});
	_ ->
	    process_flag(trap_exit, true),
	    start_server([], #state{log_match=LogMatch,
				    pid_match=PidMatch})
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get_info}, _From, State) ->
    {reply, {State#state.options, State#state.pid}, State};
handle_call({restart, ServerOpts}, _From, State) ->
    case restart_server(ServerOpts, State) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	ErrorMessage ->
	    {reply, ErrorMessage, State}
    end;
handle_call({restart}, _From, State) ->
    case restart_server([], State) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	ErrorMessage ->
	    {reply, ErrorMessage, State}
    end;    
handle_call({stop}, _From, State) ->
    {stop, asked_to_stop, State}.

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
%% handle_info({'EXIT', Port, Reason}, #state{port=Port} = State) ->
%%     {stop, {port_terminated, Reason}, State};
handle_info({Port, {data, {eol, StdOutMsg}}}, #state{port=Port} = State) ->
    parse_log_message(binary_to_list(StdOutMsg), State);
handle_info(Info, State) ->
    io:format("unrecognized info message: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate({port_terminated, _Reason}, _State) ->
    ok;
terminate(_Reason, State) ->
    kill_server(State).


%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_server(StartOpts, State) ->
    TyrantBin = proplists:get_value(tyrant_bin, StartOpts, ?TYRANT_BIN),
    case proplists:get_value(data_file, StartOpts, ?DATA_FILE) of
	"*" ->
	    DataFileBase = "\"*\"";
	"+" ->
	    DataFileBase = "\"*\"";
	OtherFile ->
	    DataFileBase = OtherFile
    end,
    TuningOpts = proplists:get_value(tuning_opts, StartOpts, ?TUNING_OPTS),
    case TuningOpts of
	[] ->
	    DataFile = DataFileBase;
	_HasTuningOpts ->
	    DataFile = DataFileBase ++ "#" ++ TuningOpts
    end,
    PortOpts = proplists:get_value(port_opts, StartOpts, ?PORT_OPTS),
    TyrantOpts = proplists:get_value(tyrant_opts, StartOpts, ?TYRANT_OPTS),
    case TyrantOpts of
	[] ->
	    TyrantCommand = TyrantBin ++ " " ++ DataFile;
	_HasTyrantOpts ->
	    TyrantCommand = TyrantBin ++ " " ++ TyrantOpts ++ " " ++ DataFile
    end,
    Port = open_port({spawn, TyrantCommand}, PortOpts),
    {ok, #state{port=Port,
		options=[{tyrant_bin, TyrantBin},
			 {data_file, DataFileBase},
			 {tuning_opts, TuningOpts},
			 {tyrant_opts, TyrantOpts}],
		pid=0,
	        log_match=State#state.log_match,
	        pid_match=State#state.pid_match}}.

restart_server(StartOpts, State) when State#state.pid > 0, State#state.port =/= nil ->
    kill_server(State),
    start_server(StartOpts, State#state{port=nil, pid=0}).

kill_server(State) when State#state.pid > 0, State#state.port =/= nil ->
    %%port_command(State#state.port, <<3:8>>),  % send ^C
    port_close(State#state.port),
    os:cmd("/bin/kill -9 " ++ integer_to_list(State#state.pid)),
    ok;
kill_server(State) when State#state.port =/= nil ->
    port_close(State#state.port);
kill_server(_State) ->
    ok.

parse_log_message(Message, State) when State#state.pid =:= 0 ->
    case re:run(Message, State#state.log_match, [{capture, all_but_first}]) of
	{match, [{MsgStart, _MsgEnd}]} ->
	    {_Head, TyrantMessage} = lists:split(MsgStart, Message),
	    error_logger:info_msg("Tyrant: ~p~n", [TyrantMessage]),
	    case re:run(TyrantMessage, State#state.pid_match, [{capture, all_but_first}]) of
		{match, [{PidStart, _PidEnd}]} ->
			{_PidHead, Pid} = lists:split(PidStart, TyrantMessage),
			{noreply, State#state{pid=list_to_integer(Pid)}};
		_ ->
			{noreply, State}
	    end;
	_ ->
	    error_logger:error_message("Unexpected Tyrant output: ~p~n", [Message]),
	    {noreply, State}
    end;
parse_log_message(Message, State) ->
    case re:run(Message, State#state.log_match, [{capture, all_but_first}]) of
	{match, [{MsgStart, _MsgEnd}]} ->
	    {_Head, TyrantMessage} = lists:split(MsgStart, Message),
	    error_logger:info_msg("Tyrant: ~p~n", [TyrantMessage]),
	    {noreply, State};
	_ ->
	    error_logger:error_message("Unexpected Tyrant output: ~p~n", [Message]),
	    {noreply, State}
    end.
