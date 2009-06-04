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
		server_pid=0,
	        log_match}).

-define(PORT_OPTS, [binary, use_stdio, stream, {line, 256}]).
-define(TYRANT_BIN, "/usr/local/bin/ttserver").
-define(TYRANT_OPTS, []).
-define(DATA_FILE, "*"). % default to in-memory hash
-define(TUNING_OPTS, []).
-define(LOG_REGEXP, "(\\S+)\\s+(\\S+)").

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
    {ok, MediciOpts} = application:get_env(options),
    {ok, RegExp} = re:compile(?LOG_REGEXP),
    process_flag(trap_exit, true),
    start_server(MediciOpts, #state{log_match=RegExp}).

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get_opts}, _From, State) ->
    {reply, {State#state.options}, State};
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
handle_info({'EXIT', Port, Reason}, #state{port=Port} = State) ->
    {stop, {port_terminated, Reason}, State};
handle_info({Port, {data, {eol, StdOutMsg}}}, #state{port=Port} = State) ->
    io:format("Got stdout: ~p~n", [StdOutMsg]),
    parse_log_message(StdOutMsg, State#state.log_match),
    {noreply, State};
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
    DataFileBase = proplists:get_value(data_file, StartOpts, ?DATA_FILE),
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
	    Port = open_port({spawn, TyrantBin ++ " " ++  
			      DataFile}, PortOpts);
	_HasTyrantOpts ->
	    Port = open_port({spawn, TyrantBin ++ " " ++ 
			      TyrantOpts ++ " " ++ 
			      DataFile}, PortOpts)
    end,
    {ok, #state{port=Port,
		options=[{tyrant_bin, TyrantBin},
			 {data_file, DataFileBase},
			 {tuning_opts, TuningOpts},
			 {tyrant_opts, TyrantOpts}],
		server_pid=0,
	        log_match=State#state.log_match}}.

restart_server(StartOpts, State) when State#state.server_pid > 0, State#state.port =/= nil ->
    kill_server(State),
    start_server(StartOpts, State#state{port=nil, server_pid=0}).

kill_server(State) when State#state.server_pid > 0, State#state.port =/= nil ->
    % close the port so we do not get an exit message
    port_close(State#state.port),
    % try a hard kill
    %os:cmd("/bin/kill -9 " ++ State#state.server_pid),
    ok;
kill_server(State) when State#state.port =/= nil ->
    port_close(State#state.port);
kill_server(_State) ->
    ok.

parse_log_message(Message, RegExp) ->
    case re:run(Message, RegExp, [{capture, all_but_first}]) of
	{match, [{MsgStart, _MsgEnd}]} ->
	    {_Head, TyrantMessage} = lists:split(MsgStart, Message),
	    error_logger:info_msg("Tyrant: ~p~n", [TyrantMessage]);
	_ ->
	    error_logger:error_message("could not parse ~p~n", [Message])
    end.
