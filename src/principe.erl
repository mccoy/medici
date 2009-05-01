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
%%% File:      principe.erl
%%% @author    Jim McCoy <mccoy@mad-scientist.com>
%%% @copyright 2009 Jim McCoy
%%% @doc ttserver binary interface 
%%%-------------------------------------------------------------------

-module(principe).
-compile([binary_comprehension]).
-export([connect/0, connect/2, put/3, putkeep/3, putcat/3, putshl/4, putnr/3,
	 out/2, get/2, mget/2, vsiz/2, iterinit/1, iternext/1, fwmkeys/3,
	 addint/3, adddouble/4, sync/1, vanish/1, rnum/1, size/1, stat/1,
	 copy/2, restore/3, setmst/3, misc/3, misc_no_update/3, ext/5]).
%%-export([table/1])  % Not tested yet

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("test/principe_test.erl").
-endif.

%% Standard definitions
-define(TSERVER, "localhost").
-define(TPORT, 1978).
-define(TIMEOUT, 5000).

%% Tyrant protocol constants
-define(PUT, 16#C810).
-define(PUTKEEP, 16#C811).
-define(PUTCAT, 16#C812).
-define(PUTSHL, 16#C813).
-define(PUTNR, 16#C818).
-define(OUT, 16#C820).
-define(GET, 16#C830).
-define(MGET, 16#C831).
-define(VSIZ, 16#C838).
-define(ITERINIT, 16#C850).
-define(ITERNEXT, 16#C851).
-define(FWMKEYS, 16#C858).
-define(ADDINT, 16#C860).
-define(ADDDOUBLE, 16#C861).
-define(EXT, 16#C868).
-define(SYNC, 16#C870).
-define(VANISH, 16#C871).
-define(COPY, 16#C872).
-define(RESTORE, 16#C873).
-define(SETMST, 16#C878).
-define(RNUM, 16#C880).
-define(SIZE, 16#C881).
-define(STAT, 16#C888).
-define(MISC, 16#C890).

-define(MONOULOG, 1 bsl 0).
-define(XOLCKREC, 1 bsl 0).
-define(XOLCKGLB, 1 bsl 1).

%% Some function patterns that are used frequently.  For all tyrant functions the
%% expected type for keys and values is iolist().
-define(T0(Code), gen_tcp:send(Socket, [<<Code:16>>])).
-define(T1(Code), gen_tcp:send(Socket, [<<Code:16>>, <<(iolist_size(Key)):32>>, Key])).
-define(T2(Code), gen_tcp:send(Socket, [<<Code:16>>, <<(iolist_size(Key)):32>>, <<(iolist_size(Value)):32>>, Key, Value])).
-define(R_SUCCESS, tyrant_response(Socket, fun recv_success/2)).
-define(R_SIZE, tyrant_response(Socket, fun recv_size/2)).
-define(R_SIZE_DATA, tyrant_response(Socket, fun recv_size_data/2)).
-define(R_SIZE64, tyrant_response(Socket, fun recv_size64/2)).

%% The Tokyo Tyrant access functions
connect() ->
    connect(?TSERVER, ?TPORT).

connect(Server, Port) ->
    gen_tcp:connect(Server, Port, [binary, 
				   {packet, 0}, 
				   {nodelay, true}, 
				   {reuseaddr, true}, 
				   {active, true},
				   {keepalive, true}]).

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

%% Store a new value for the given key
put(Socket, Key, Value) ->
    ?T2(?PUT),							     
    ?R_SUCCESS.

%% Put a new key/value pair into the remote database
putkeep(Socket, Key, Value) ->
    ?T2(?PUTKEEP),
    ?R_SUCCESS.

%% Concatenate a value to the end of the current value for a given key
putcat(Socket, Key, Value) ->
    ?T2(?PUTCAT),
    ?R_SUCCESS.

%% Concatenate a value to a given key and shift it to the left
putshl(Socket, Key, Value, Width) when is_integer(Width) ->
    gen_tcp:send(Socket, [<<?PUTSHL:16>>, 
			  <<(iolist_size(Key)):32>>, 
			  <<(iolist_size(Value)):32>>, 
			  <<Width:32>>, Key, Value]),
    ?R_SUCCESS.

%% Put a key/value pair to the remote database and do not wait for a response
putnr(Socket, Key, Value) ->
    ?T2(?PUTNR),
    ok.

%% Remove a key from the remote database
out(Socket, Key) ->
    ?T1(?OUT),
    ?R_SUCCESS.

%% Get the value for a given key
get(Socket, Key) ->
    ?T1(?GET),
    ?R_SIZE_DATA.

%% Get the values for a list of keys
mget(Socket, KeyList) when is_list(KeyList) ->
    gen_tcp:send(Socket, [<<?MGET:16>>, 
			  <<(length(KeyList)):32>>, 
			  [[<<(iolist_size(Key)):32>>, Key] || Key <- KeyList]
			 ]),
    tyrant_response(Socket, fun recv_count_4tuple/2).

%% Get the size of the value for a given key
vsiz(Socket, Key) ->
    ?T1(?VSIZ),
    ?R_SIZE.

%% Start iteration protocol
iterinit(Socket) ->
    ?T0(?ITERINIT),
    ?R_SUCCESS.

%% Get the next key/value pair in the iteration protocol
iternext(Socket) ->
    ?T0(?ITERNEXT),
    ?R_SIZE_DATA.

%% Return a number of records that match a given prefix
fwmkeys(Socket, Prefix, MaxKeys) when is_integer(MaxKeys) ->
    gen_tcp:send(Socket, [<<?FWMKEYS:16>>, 
			  <<(iolist_size(Prefix)):32>>, 
			  <<MaxKeys:32>>, Prefix]),
    tyrant_response(Socket, fun recv_count_2tuple/2).

%% Add an integer value to the existing value of a key
addint(Socket, Key, Int) when is_integer(Int) ->
    gen_tcp:send(Socket, [<<?ADDINT:16>>, <<(iolist_size(Key)):32>>, <<Int:32>>, Key]),
    ?R_SIZE.

%% Add a float to the existing value of a key
adddouble(Socket, Key, Integral, Fractional) when is_binary(Key), is_integer(Integral), is_integer(Fractional) ->
    gen_tcp:send(Socket, [<<?ADDDOUBLE:16>>, <<(iolist_size(Key)):32>>, 
			  <<Integral:64>>, <<Fractional:64>>, Key]),
    tyrant_response(Socket, fun recv_size64_size64/2).    

%% Call sync() on the remote database
sync(Socket) ->
    ?T0(?SYNC),
    ?R_SUCCESS.

%% Remove all records from the remote database
vanish(Socket) ->
    ?T0(?VANISH),
    ?R_SUCCESS.

%% Get the number of records in the remote database
rnum(Socket) ->
    ?T0(?RNUM),
    ?R_SIZE64.

%% Get the size in bytes of the remote database
size(Socket) ->
    ?T0(?SIZE),
    ?R_SIZE64.

%% Get the status string of a remote database
stat(Socket) ->
    ?T0(?STAT),
    StatString = ?R_SIZE_DATA,
    case StatString of
	{error, Reason} ->
	    {error, Reason};
	GoodStat ->
	    stat_to_proplist(GoodStat)
    end.

stat_to_proplist(StatBin) ->
    stat_to_proplist(string:tokens(binary_to_list(StatBin), "\n\t"), []).

stat_to_proplist([], Acc) ->
    Acc;
stat_to_proplist([H1, H2 | T], Acc) ->
    stat_to_proplist(T, [{list_to_atom(H1), H2} | Acc]).

%% Make a copy of the database file of the remote database
copy(Socket, Key) when is_binary(Key) ->
    ?T1(?COPY), % Using 'Key' so that the macro binds properly...
    ?R_SUCCESS.

%% Restore the database to a particular point in time from the update log
restore(Socket, PathName, TimeStamp) ->
    gen_tcp:send(Socket, [<<?RESTORE:16>>, 
			  <<(iolist_size(PathName)):32>>,
			  <<TimeStamp:64>>, 
			  PathName]),
    ?R_SUCCESS.

%% Set the replication master of a remote database server
setmst(Socket, HostName, Port) when is_integer(Port) ->
    gen_tcp:send(Socket, [<<?SETMST:16>>, 
			  <<(iolist_size(HostName)):32>>, 
			  <<Port:32>>, HostName]),
    ?R_SUCCESS.

%% Tyrant misc() call that writes to the update logs
misc(Socket, Func, Args) when length(Args) > 0 ->
    %% All database types support putlist, outlist, and getlist.
    %%    putlist -> store records, Args is list of sequential keys and values, returns []
    %%    outlist -> remove records, Args is list of keys, returns []
    %%    getlist -> retrieve records, args is list of keys, returns list of values
    %% Table database supports setindex, search, and genuid.
    %%    setindex -> set the column index, Arg is name of col and type of col data, returns success val
    %%    search -> run a search on the columns, returns list of values
    %%    genuid -> generate unique ID number, returns integer
    gen_tcp:send(Socket, [<<?MISC:16>>, 
			  <<(iolist_size(Func)):32>>, <<0:32>>, 
			  <<(length(Args)):32>>, 
			  Func,
			  [[<<(iolist_size(Arg)):32>>, Arg] || Arg <- Args ]
			 ]),
    tyrant_response(Socket, fun recv_count_2tuple/2);
misc(Socket, Func, _Args) ->
    gen_tcp:send(Socket, [<<?MISC:16>>, 
			  <<(iolist_size(Func)):32>>, <<0:32>>, 
			  <<0:32>>, 
			  Func]),
    tyrant_response(Socket, fun recv_count_2tuple/2).


%% Tyrant misc() call that does not write to the update logs
misc_no_update(Socket, Func, Args) when length(Args) > 0 ->
    gen_tcp:send(Socket, [<<?MISC:16>>, 
			  <<(iolist_size(Func)):32>>, <<1:32>>, 
			  <<(length(Args)):32>>,
			  Func,
			  [[<<(iolist_size(Arg)):32>>, Arg] || Arg <- Args ]
			 ]),
    tyrant_response(Socket, fun recv_count_2tuple/2);
misc_no_update(Socket, Func, _Args) ->
    gen_tcp:send(Socket, [<<?MISC:16>>, 
			  <<(iolist_size(Func)):32>>, <<1:32>>, 
			  <<0:32>>, 
			  Func]),
    tyrant_response(Socket, fun recv_count_2tuple/2).

%% Call a a function of the Tyrant script language extensions.
ext(Socket, Func, Opts, Key, Value) when is_binary(Key), is_binary(Value) ->
    %% TODO: Opts needs to be parsed.  Probably as a proplist [record_lock, global_lock, neither...]
    gen_tcp:send(Socket, [<<?EXT:16>>, <<(iolist_size(Func)):32>>, <<Opts:32>>, 
			  <<(iolist_size(Key)):32>>, <<(iolist_size(Value)):32>>, 
			  Func, Key, Value]),
    ?R_SUCCESS.

%%====================================================================
%% Handle response from the server
%%====================================================================

tyrant_response(Socket, ResponseHandler) ->
    receive
        {tcp, Socket, <<ErrorCode:8, _Rest/binary>>} when ErrorCode =/= 0 ->
	    {error, ErrorCode};
        {tcp_closed, Socket} -> 
	    {error, conn_closed};
        {tcp_error, Socket, _Reason} -> 
	    {error, conn_error};
        Data -> 
	    ResponseHandler(Socket, Data)
    after ?TIMEOUT -> 
	    {error, timeout}
    end.

%% receive 8-bit success flag
recv_success(_Socket, {tcp, _, <<0:8>>}) -> 
    ok.
 
%% receive 8-bit success flag + 32-bit int
recv_size(_Socket, {tcp, _, <<0:8, ValSize:32>>}) -> 
    ValSize.
 
%% receive 8-bit success flag + 64-bit int
recv_size64(_Socket, {tcp, _, <<0:8, ValSize:64>>}) -> 
    ValSize.
 
%% receive 8-bit success flag + 64-bit int + 64-bit int
recv_size64_size64(_Socket, {tcp, _, <<0:8, V1:64, V2:64>>}) -> 
    {V1, V2}.
 
%% receive 8-bit success flag + length1 + data1
recv_size_data(Socket, Data) ->
    case Data of
        {tcp, _, <<0:8, Length:32, Rest/binary>>} ->
            {Value, <<>>} = recv_until(Socket, Rest, Length),
            Value
    end.

%% receive 8-bit success flag + count + (length1, length2, data1, data2)*count
recv_count_4tuple(Socket, Data) ->
    case Data of
        {tcp, _, <<0:8, 0:32, _Rest/binary>>} ->
            [];
        {tcp, _, <<0:8, RecCnt:32, Rest/binary>>} ->
            {KVS, _} = lists:mapfoldl(
                            fun(_N, Acc) ->
                                <<KeySize:32, ValSize:32, Bin/binary>> = Acc,
                                {Key, Rest1} = recv_until(Socket, Bin, KeySize),
                                {Value, Rest2} = recv_until(Socket, Rest1, ValSize),
                                {{Key, Value}, Rest2}
                            end, 
                            Rest, lists:seq(1, RecCnt)
                        ),
            KVS
    end.

%% receive 8-bit success flag + count + (length1, data1)*count
recv_count_2tuple(Socket, Data) ->
    case Data of
        {tcp, _, <<0:8, 0:32, _Rest/binary>>} ->
	    [];
        {tcp, _, <<0:8, Cnt:32, Rest/binary>>} ->
            {Keys, _} = lists:mapfoldl(
                            fun(_N, Acc) ->
                                <<KeySize:32, Bin/binary>> = Acc,
                                recv_until(Socket, Bin, KeySize)
                            end,
                            Rest, lists:seq(1, Cnt)
                        ),
            Keys
    end.
 
%% receive length-delimited data that may require multiple pulls from the socket
recv_until(Socket, Bin, ReqLength) when byte_size(Bin) < ReqLength ->
    receive
        {tcp, Socket, Data} ->
            Combined = <<Bin/binary, Data/binary>>,
            recv_until(Socket, Combined, ReqLength);
        {tcp_closed, Socket} -> 
	    {error, conn_closed};
	{error, closed} ->
	    conn_closed
    after ?TIMEOUT -> timeout
    end;    
recv_until(_Socket, Bin, ReqLength) when byte_size(Bin) =:= ReqLength ->
    {Bin, <<>>};
recv_until(_Socket, Bin, ReqLength) when byte_size(Bin) > ReqLength ->
    <<Required:ReqLength/binary, Rest/binary>> = Bin,
    {Required, Rest}.
 
