%%%-------------------------------------------------------------------
%%% File    : medici_sup.erl
%%% Author  : Jim McCoy <>
%%% Description : 
%%%
%%% Created :  6 May 2009 by Jim McCoy <>
%%%-------------------------------------------------------------------
-module(medici_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).


%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link(?MODULE, []).

start_link(MediciProps) ->
    supervisor:start_link(?MODULE, MediciProps).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init(MediciProps) ->
    MediciClientSupervisor = {"ClientSupervisor",
			      {'medici_client_sup', start_link, MediciProps},
			      permanent, 
			      infinity, 
			      supervisor, 
			      ["medici_client_sup"]},
    MediciController = {"Controller",
			{'medici_controller', start_link, MediciProps},
			permanent,
			2000,
			worker,
			["medici_controller"]},

    {ok,{{one_for_all,1,10}, [MediciClientSupervisor, MediciController]}}.

%%====================================================================
%% Internal functions
%%====================================================================
