%%%-------------------------------------------------------------------
%%% File    : medici.erl
%%% Author  : Jim McCoy <>
%%% Description : 
%%%
%%% Created :  7 May 2009 by Jim McCoy <>
%%%-------------------------------------------------------------------
-module(medici).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Basic API exports
-export([put/2, putkeep/2, putcat/2, putshl/3, putnr/2, out/1, get/1, 
	 mget/1, vsiz/1, iterinit/0, iternext/0, fwmkeys/2, addint/2,
	 adddouble/2, adddouble/3, sync/0, vanish/0, rnum/0, size/0, 
	 stat/0, copy/1, restore/2, setmst/2]).

%% Table API exports
-export([update/2, setindex/2, genuid/0, query_limit/2, query_limit/3,
	 query_add_condition/4, query_order/3, search/1, searchcount/1,
	 searchout/1]).


%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
    medici_sup:start_link(StartArgs).

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Medici API
%%====================================================================

%%--------------------------------------------------------------------
%% These API calls assume you are using the default registered name
%% for the service and that you know what sort of reomte database
%% you are talking to (e.g. don't make table-specific calls to a hash
%% database and if you need byte-order specific ops then you should
%% update these or make your own versions to make the appropriate calls
%% to the controller.)
%%--------------------------------------------------------------------

put(Key, Value) ->
    gen_server:call(medici, {put, Key, Value}).
 
putcat(Key, Value) ->
    gen_server:call(medici, {putcat, Key, Value}).

putkeep(Key, Value) ->
    gen_server:call(medici, {putkeep, Key, Value}).

putshl(Key, Value, Width) ->
    gen_server:call(medici, {putshl, Key, Value, Width}).

putnr(Key, Value) ->
    gen_server:cast(medici, {putnr, Key, Value}).

out(Key) ->
    gen_server:call(medici, {out, Key}).

get(Key) ->
    gen_server:call(medici, {get, Key}).

mget(KeyList) ->
    gen_server:call(medici, {mget, KeyList}).

vsiz(Key) ->
    gen_server:call(medici, {vsiz, Key}).

iterinit() ->
    gen_server:call(medici, {iterinit}).

iternext() ->
    gen_server:call(medici, {iternext}).

fwmkeys(Prefix, MaxKeys) ->
    gen_server:call(medici, {fwmkeys, Prefix, MaxKeys}).

addint(Key, Int) ->
    gen_server:call(medici, {addint, Key, Int}).

adddouble(Key, Double) ->
    gen_server:call(medici, {adddouble, Key, Double}).

adddouble(Key, IntPart, FracPart) ->
    gen_server:call(medici, {adddouble, Key, IntPart, FracPart}).

sync() ->
    gen_server:call(medici, {sync}).

vanish() ->
    gen_server:call(medici, {vanish}).

rnum() ->
    gen_server:call(medici, {rnum}).

size() ->
    gen_server:call(medici, {size}).

stat() ->
    gen_server:call(medici, {stat}).

copy(PathName) ->
    gen_server:call(medici, {copy, PathName}).

restore(PathName, TimeStamp) ->
    gen_server:call(medici, {restore, PathName, TimeStamp}).

setmst(HostName, Port) ->
    gen_server:call(medici, {setmst, HostName, Port}).

%% Additional table functions
update(Key, NewCols) ->
    gen_server:call(medici, {update, Key, NewCols}).

setindex(Column, Type) ->
    gen_server:call(medici, {setindex, Column, Type}).

genuid() ->
    gen_server:call(medici, {genuid}).

query_limit(OldQuery, Max) ->
    gen_server:call(medici, {query_limit, OldQuery, Max}).

query_limit(OldQuery, Max, Skip) ->
    gen_server:call(medici, {query_limit, OldQuery, Max, Skip}).

query_add_condition(OldQuery, Column, Op, ExprList) ->
    gen_server:call(medici, {query_add_condition, OldQuery, Column, Op, ExprList}).

query_order(OldQuery, Column, Type) ->
    gen_server:call(medici, {query_order, OldQuery, Column, Type}).

search(Query) ->
    gen_server:call(medici, {search, Query}).

searchcount(Query) ->
    gen_server:call(medici, {searchcount, Query}).

searchout(Query) ->
    gen_server:call(medici, {searchout, Query}).



%%====================================================================
%% Internal functions
%%====================================================================
