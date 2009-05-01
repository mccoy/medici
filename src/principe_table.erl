%% Copyright 2009, Jim McCoy <mccoy@mad-scientist.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
 
%%%-------------------------------------------------------------------
%%% File:      principe_table.erl
%%% @author    Jim McCoy <mccoy@mad-scientist.com>
%%% @copyright 2009 Jim McCoy
%%% @doc ttserver binary interface for tables
%%%-------------------------------------------------------------------

-module(principe_table).
-compile([export_all,binary_comprehension]).
%% -export([connect/0, connect/2, put/3, putkeep/3, putcat/3, out/2, get/2,
%% 	 vsiz/2, iterinit/1, iternext/1, fwmkeys/3, sync/1, vanish/1,
%% 	 rnum/1, size/1, stat/1, copy/2, restore/3, addint/3, adddouble/4,
%% 	 setmst/3, setindex/3, query_set_limit/3, query_set_limit/2,
%% 	 query_add_condition/4, query_set_order/3, search/2, genuid/1,
%% 	 searchcount/2, searchout/2, encode_table/1, decode_table/1,
%% 	 convert_cols/2]).
%%-export([table/1])  % Not tested yet

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test/principe_table_test.erl").
-endif.

-define(NULL, <<0:8>>).

%% Constants for tyrant tables
-define(ITLEXICAL, "0").
-define(ITDECIMAL, "1").
-define(ITOPT, "9998").
-define(ITVOID, "9999").

-define(QCSTREQ, 0).
-define(QCSTRINC, 1).
-define(QCSTRBW, 2).
-define(QCSTREW, 3).
-define(QCSTRAND, 4).
-define(QCSTROR, 5).
-define(QCSTROREQ, 6).
-define(QCSTRRX, 7).
-define(QCNUMEQ, 8).
-define(QCNUMGT, 9).
-define(QCNUMGE, 10).
-define(QCNUMLT, 11).
-define(QCNUMLE, 12).
-define(QCNUMBT, 13).
-define(QCNUMOREQ, 14).
-define(QCNEGATE, 1 bsl 24).
-define(QCNOIDX, 1 bsl 25).

-define(QOSTRASC, 0).
-define(QOSTRDESC, 1).
-define(QONUMASC, 2).
-define(QONUMDESC, 3).

%% Some function patterns that are used frequently
-define(TT(Func, Args), principe:misc(Socket, Func, Args)).

%% The Tokyo Tyrant access functions
connect() ->
    principe:connect().

connect(Server, Port) ->
    principe:connect(Server, Port).

%% table(Socket) ->
%%     TF = fun() -> qlc_next(firstitem(Socket)) end,
%%     InfoFun = fun(num_of_objects) -> principe:rnum(Socket);
%%                  (keypos) -> 1;
%%                  (is_sorted_key) -> false;
%%                  (is_unique_objects) -> true;
%%                  (_) -> undefined
%%               end,
%%     LookupFun =
%%         fun(1, Ks) ->
%%                 principe:mget(Socket, Ks)
%%         end,
%%     qlc:table(TF, [{info_fun, InfoFun}, {lookup_fun, LookupFun},{key_equality,'=='}]).

%% %% Helper functions for the qlc_next function
%% firstitem(Socket) ->
%%     ok = principe:iterinit(Socket),
%%     case principe:iternext(Socket) of
%% 	{error, _ErrCode} ->
%% 	    none;
%% 	Key ->
%% 	    {Key, principe:get(Socket, Key), Socket}
%%     end.
%% nextitem({_K, _V, Socket}) ->
%%     case principe:iternext(Socket) of
%% 	{error, _ErrCode} ->
%% 	    none;
%% 	Key ->
%% 	    {Key, principe:get(Socket, Key), Socket}
%%     end.

%% %% The traversal function used by table/1
%% qlc_next({X, V, S}) ->
%%     [{X,V} | fun() -> qlc_next(nextitem({X, V, S})) end];
%% qlc_next(none) ->
%%     [].

%%====================================================================
%%  Standard tyrant functions
%%====================================================================

%% Add an integer value to the existing value of a key, value is added
%% column named "_num", which is created if it does not exist.
addint(Socket, Key, Int) ->
    principe:addint(Socket, Key, Int).

%% Add a float value to the existing value of a key, value is added
%% column named "_num", which is created if it does not exist.
adddouble(Socket, Key, Integral, Fractional) ->
    principe:adddouble(Socket, Key, Integral, Fractional).    

%% Start iteration protocol
iterinit(Socket) ->
    principe:iterinit(Socket).

%% Get the next key/value pair in the iteration protocol
iternext(Socket) ->
    principe:iternext(Socket).

%% Return a number of records that match a given prefix
fwmkeys(Socket, Prefix, MaxKeys) ->
    principe:fwmkeys(Socket, Prefix, MaxKeys).

%% Get the size of the value associated with a given key.
vsiz(Socket, Key) ->
    principe:vsiz(Socket, Key).

%% Call sync() on the remote database
sync(Socket) ->
    principe:sync(Socket).

%% Remove all records from the remote database
vanish(Socket) ->
    principe:vanish(Socket).

%% Get the number of records in the remote database
rnum(Socket) ->
    principe:rnum(Socket).

%% Get the size in bytes of the remote database
size(Socket) ->
    principe:size(Socket).

%% Get the status string of a remote database
stat(Socket) ->
    principe:stat(Socket).

%% Make a copy of the database file of the remote database
copy(Socket, PathName) ->
    principe:copy(Socket, PathName).

%% Restore the database to a particular point in time from the update log
restore(Socket, PathName, TimeStamp) ->
    principe:restore(Socket, PathName, TimeStamp).

%% Set the replication master of a remote database server
setmst(Socket, HostName, Port) ->
    principe:setmst(Socket, HostName, Port).

%%====================================================================
%%  Table functions
%%====================================================================

%%% Store a value for a given key.
put(Socket, Key, Cols) ->
    Data = encode_table(Cols),
    ?TT(<<"put">>, [Key | Data]).

%% Store a new record into the database
putkeep(Socket, Key, Cols) ->
    Data = encode_table(Cols),
    principe:misc(Socket, <<"putkeep">>, [Key | Data]).

%% Concatenate a set of column values to the existing value of Key.
putcat(Socket, Key, Cols) ->
    Data = encode_table(Cols),
    principe:misc(Socket, <<"putcat">>, [Key | Data]).

%% Remove a key & value from the table.
out(Socket, Key) ->
    principe:misc(Socket, <<"out">>, [Key]).

%% Get the value for a given key.
get(Socket, Key) ->
    principe:misc(Socket, <<"get">>, [Key]).

mget(Socket, KeyList) ->
    principe:misc(Socket, <<"getlist">>, [KeyList]).

%% Tell the tyrant server to build an index for a column.  The ColName
%% should be either the atom "primary" or a binary, Type should be an atom.
setindex(Socket, primary, Type) when is_atom(Type) ->
    principe:misc(Socket, <<"setindex">>, [?NULL, setindex_request_val(Type)]);
setindex(Socket, ColName, Type) when is_atom(Type) ->
    principe:misc(Socket, <<"setindex">>, [ColName, setindex_request_val(Type)]).

%% Generate a unique id within the set of primary keys
genuid(Socket) ->
    principe:misc(Socket, <<"genuid">>, []).

%% Add a condition for a query.  ExprList should be a list of one or more
%% values where each value is either a binary, list, or integer.  Op can be
%% either an atom or a tuple of atoms describing the operation.  If the first
%% atom in an Op tuple is "no" then the condition is a negation query and if
%% the last atom is no_index an existing index on the remote database server will
%% be bypassed.
query_add_condition(Query, ColName, Op, ExprList) when is_list(ExprList) ->
    [{add_cond, {ColName, <<(add_condition_op_val(Op)):32>>, convert_query_exprlist(ExprList)}} | Query].

%% Set a limit on the number of returned values
query_set_limit(Query, Max, Skip) when is_integer(Max), is_integer(Skip) ->
    case proplists:is_defined(set_limit, Query) of
	true ->
	    ClearedQuery = proplists:delete(set_limit, Query),
	    [{set_limit, {integer_to_list(Max), integer_to_list(Skip)}} | ClearedQuery];
	false ->
	    [{set_limit, {integer_to_list(Max), integer_to_list(Skip)}} | Query]
    end.
%%% XXX: should the missing skip be 0 or -1 (protocol ref and perl versions seem to disagree)
query_set_limit(Query, Max) ->
    query_set_limit(Query, Max, 0).

%% Set the order for returned values
query_set_order(Query, primary, Type) when is_atom(Type) ->
    case proplists:is_defined(set_order, Query) of
	true ->
	    ClearedQuery = proplists:delete(set_order, Query),
	    [{set_order, {?NULL, order_request_val(Type)}} | ClearedQuery];
	false ->
	    [{set_order, {?NULL, order_request_val(Type)}} | Query]
    end;
query_set_order(Query, ColName, Type) when is_atom(Type) ->
    case proplists:is_defined(set_order, Query) of
	true ->
	    ClearedQuery = proplists:delete(set_order, Query),
	    [{set_order, {ColName, order_request_val(Type)}} | ClearedQuery];
	false ->
	    [{set_order, {ColName, order_request_val(Type)}} | Query]
    end.

%% Run a prepared query against the table and return matching keys.
search(_Socket, TblQuery) ->
    SearchQuery = query_to_argslist(TblQuery),
    principe:misc(<<"search">>, SearchQuery).

%% Run a prepared query against the table and get the count of matching keys.
searchcount(Socket, TblQuery) ->
    SearchQuery = query_to_argslist(TblQuery),
    CountQuery = [SearchQuery | <<"count">>],
    principe:misc(Socket, <<"search">>, CountQuery).

%% %% Run a prepared query against the table and get the matching records.  Due
%% %% to protocol restraints, the returned result cannot include columns whose
%% %% name or value include the null (0x0) character.
%% tblsearchget(Socket, TblQuery) ->
%%     void.

%% no_nulls(Binary) when is_binary(Binary) ->
%%     no_nulls(binary_to_list(Binary));
%% no_nulls(List) when is_list(List) ->
%%     not(lists:member(0, List)).

%% Run a prepared query against the table and remove the matching records
searchout(Socket, TblQuery) ->
    SearchQuery = query_to_argslist(TblQuery),
    OutQuery = [SearchQuery | <<"out">>],
    principe:misc(Socket, <<"search">>, OutQuery).

%% tblrescols(Socket, TblQuery) ->
%%     void.

 
%%====================================================================
%%  Table utility functions
%%====================================================================

add_condition_op_val({no, Op}) when is_atom(Op) ->
    ?QCNEGATE bor add_condition_op_val(Op);
add_condition_op_val({Op, no_index}) when is_atom(Op) ->
    ?QCNOIDX bor add_condition_op_val(Op);
add_condition_op_val({no, Op, no_index}) when is_atom(Op)->
    ?QCNEGATE bor ?QCNOIDX bor add_condition_op_val(Op);
add_condition_op_val(Op) when is_atom(Op) ->
    case Op of
	str_eq ->
	    ?QCSTREQ;
	str_inc ->
	    ?QCSTRINC;
	str_begin ->
	    ?QCSTRBW;
	str_end ->
	    ?QCSTREW;
	str_and ->
	    ?QCSTRAND;
	str_or ->
	    ?QCSTROR;
	str_regex ->
	    ?QCSTRRX;
	num_eq ->
	    ?QCNUMEQ;
	num_gt ->
	    ?QCNUMGT;
	num_ge ->
	    ?QCNUMGE;
	num_lt ->
	    ?QCNUMLT;
	num_le ->
	    ?QCNUMLE;
	num_between ->
	    ?QCNUMBT;
	num_in_list ->
	    ?QCNUMOREQ
    end.

setindex_request_val(Type) ->
    case Type of
	lexical ->
	    ?ITLEXICAL;
	decimal ->
	    ?ITDECIMAL;
	optimized ->
	    ?ITOPT;
	void ->
	    ?ITVOID
    end.

order_request_val(Type) ->
    case Type of
	str_ascending ->
	    ?QOSTRASC;
	str_descending ->
	    ?QOSTRDESC;
	num_ascending ->
	    ?QONUMASC;
	num_descending ->
	    ?QONUMDESC
    end.

convert_query_exprlist(ExprList) ->
    convert_query_exprlist(ExprList, []).

convert_query_exprlist([H | T], []) when is_integer(H) ->
    convert_query_exprlist(T, [integer_to_list(H)]);
convert_query_exprlist([H | T], []) ->
    convert_query_exprlist(T, [H]);
convert_query_exprlist([H | T], Acc) when is_integer(H) ->
    convert_query_exprlist(T, [integer_to_list(H) | ["," | Acc]]);
convert_query_exprlist([H | T], Acc) ->
    convert_query_exprlist(T, [H | ["," | Acc]]);
convert_query_exprlist([], Acc) ->
    lists:reverse(Acc).

query_to_argslist(QueryProplist) ->
    query_to_argslist(QueryProplist, []).

query_to_argslist([{K, V} | T], BinArgs) ->
    case K of
	add_cond ->
	    {ColName, Op, ExprList} = V,
	    query_to_argslist(T, [["addcond", ?NULL, ColName, ?NULL, Op, ?NULL, ExprList] | BinArgs]);
	set_limit ->
	    {M, S} = V,
	    query_to_argslist(T, [["setlimit", ?NULL, <<M:32>>, ?NULL, <<S:32>>] | BinArgs]);
	order_by ->
            {ColName, Type} = V,
	    query_to_argslist(T, [["setorder", ?NULL, ColName, ?NULL, Type] | BinArgs])
    end;
query_to_argslist([], BinArgs) ->
    lists:reverse(BinArgs).

encode_table(Data) when is_list(Data) ->
    encode_table(Data, []).

encode_table([], Acc) ->
    lists:reverse(Acc);
encode_table([{K, V} | Tail], Acc) ->
    encode_table(Tail, [V | [ K | Acc]]).

decode_table({error, Code}) ->
    {error, Code};
decode_table(Data) when is_list(Data) ->
    decode_table(Data, []).

decode_table([], Acc) ->
    lists:reverse(Acc);
decode_table([K, V | Tail], Acc) ->
    decode_table(Tail, [{K, V} | Acc]).

