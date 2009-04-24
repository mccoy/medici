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
%%% @doc ttserver connection handler 
%%%-------------------------------------------------------------------

-module(principe).
-compile([binary_comprehension]).
-export([connect/0, connect/2, put/3, putkeep/3, putcat/3, putshl/4, putnr/3,
	 out/2, get/2, mget/2, vsiz/2, iterinit/1, iternext/1, fwmkeys/3,
	 addint/3, adddouble/4, sync/1, vanish/1, rnum/1, size/1, stat/1,
	 copy/2, restore/3, setmst/3, misc/3, misc_no_update/3, ext/5]).
%%-export([table/1])  % Not tested yet

%% Standard definitions
-define(TSERVER, "localhost").
-define(TPORT, 1978).
-define(TIMEOUT, 5000).

%% Tyrant protocol details
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

-define(T0(Code), gen_tcp:send(Socket, <<Code:16>>)).
-define(T1(Code), gen_tcp:send(Socket, <<Code:16, (byte_size(Key)):32, Key/bytes>>)).
-define(T2(Code), gen_tcp:send(Socket, <<Code:16, (byte_size(Key)):32, (byte_size(Value)):32, Key/bytes, Value/bytes>>)).
-define(R_SUCCESS, tyrant_response(Socket, fun recv_success/2)).
-define(R_SIZE, tyrant_response(Socket, fun recv_size/2)).
-define(R_SIZE_DATA, tyrant_response(Socket, fun recv_size_data/2)).
-define(R_SIZE64, tyrant_response(Socket, fun recv_size64/2)).

connect() ->
    connect(?TSERVER, ?TPORT).

connect(Server, Port) ->
    gen_tcp:connect(Server, Port, [binary, {packet, 0}, {nodelay, true}, {reuseaddr, true}, {active, true}]).

%% table(Socket) ->
%%     TF = fun() -> qlc_next(traverse(Socket)) end,
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

%% %% Traverse is a helper for the table function, similar to gb_trees:next/1
%% traverse(Socket) when is_port(Socket) ->
%%     ok = principe:iterinit(Socket),
%%     traverse({nil, nil, Socket});
%% traverse({_K, _V, Socket}) ->
%%     case principe:iternext(Socket) of
%% 	{error, _ErrCode} ->
%% 	    none;
%% 	Key ->
%% 	    {Key, principe:get(Socket, Key), Socket}
%%     end.

%% %% The traversal function used by table/1
%% qlc_next({X, V, S}) ->
%%     [{X,V} | fun() -> qlc_next(traverse({X, V, S})) end];
%% qlc_next(none) ->
%%     [].

put(Socket, Key, Value) when is_binary(Key), is_binary(Value) ->
    ?T2(?PUT),
    ?R_SUCCESS.

putkeep(Socket, Key, Value) when is_binary(Key), is_binary(Value) ->
    ?T2(?PUTKEEP),
    ?R_SUCCESS.

putcat(Socket, Key, Value) when is_binary(Key), is_binary(Value) ->
    ?T2(?PUTCAT),
    ?R_SUCCESS.

putshl(Socket, Key, Value, Width) when is_binary(Key), is_binary(Value), is_integer(Width) ->
    gen_tcp:send(Socket, <<?PUTSHL:16, (byte_size(Key)):32, (byte_size(Value)):32, Width:32, Key/bytes, Value/bytes>>),
    ?R_SUCCESS.

putnr(Socket, Key, Value) when is_binary(Key), is_binary(Value) ->
    ?T2(?PUTNR),
    ok.

out(Socket, Key) when is_binary(Key) ->
    ?T1(?OUT),
    ?R_SUCCESS.

get(Socket, Key) when is_binary(Key) ->
    ?T1(?GET),
    ?R_SIZE_DATA.

mget(Socket, KeyList) when is_list(KeyList) ->
    DataList = << << (byte_size(Key)):32, Key/bits >> || Key <- KeyList >>,
    gen_tcp:send(Socket, <<?MGET:16, (length(KeyList)):32, DataList/bytes>>),
    tyrant_response(Socket, fun recv_count_4tuple/2).

vsiz(Socket, Key) when is_binary(Key) ->
    ?T1(?VSIZ),
    ?R_SIZE.

iterinit(Socket) ->
    ?T0(?ITERINIT),
    ?R_SUCCESS.

iternext(Socket) ->
    ?T0(?ITERNEXT),
    ?R_SIZE_DATA.

fwmkeys(Socket, Prefix, MaxKeys) when is_binary(Prefix), is_integer(MaxKeys) ->
    gen_tcp:send(Socket, <<?FWMKEYS:16, (byte_size(Prefix)):32, MaxKeys:32, Prefix/bytes>>),
    tyrant_response(Socket, fun recv_count_2tuple/2).

addint(Socket, Key, Int) when is_binary(Key), is_integer(Int) ->
    gen_tcp:send(Socket, <<?ADDINT:16, (byte_size(Key)):32, Int:32, Key/bytes>>),
    ?R_SIZE.

adddouble(Socket, Key, Integral, Fractional) when is_binary(Key), is_integer(Integral), is_integer(Fractional) ->
    gen_tcp:send(Socket, <<?ADDDOUBLE:16, (byte_size(Key)):32, Integral:64, Fractional:64, Key/bytes>>),
    tyrant_response(Socket, fun recv_size64_size64/2).    

sync(Socket) ->
    ?T0(?SYNC),
    ?R_SUCCESS.

vanish(Socket) ->
    ?T0(?VANISH),
    ?R_SUCCESS.

rnum(Socket) ->
    ?T0(?RNUM),
    ?R_SIZE64.

size(Socket) ->
    ?T0(?SIZE),
    ?R_SIZE64.

stat(Socket) ->
    ?T0(?STAT),
    ?R_SIZE_DATA.

copy(Socket, Key) when is_binary(Key) ->
    ?T1(?COPY),
    ?R_SUCCESS.

restore(Socket, PathName, TimeStamp) when is_binary(PathName) ->
    gen_tcp:send(Socket, <<?RESTORE:16, (byte_size(PathName)):32, TimeStamp:64, PathName/bits>>),
    ?R_SUCCESS.

setmst(Socket, HostName, Port) when is_binary(HostName), is_integer(Port) ->
    gen_tcp:send(Socket, <<?SETMST:16, (byte_size(HostName)):32, Port:32, HostName/bits>>),
    ?R_SUCCESS.

misc(Socket, Func, Args) ->
    %% Tyrant misc() call that writes to the update logs
    ArgData = << << (byte_size(Arg)):32, Arg/bits >> || Arg <- Args >>,
    gen_tcp:send(Socket, <<?MISC:16, (length(Func)):32, 0:32, (length(Args)):32, ArgData/bytes>>),
    tyrant_response(Socket, fun recv_count_2tuple/2).

misc_no_update(Socket, Func, Args) ->
    %% Tyrant misc() call that does not write to the update logs
    ArgData = << << (byte_size(Arg)):32, Arg/bits >> || Arg <- Args >>,
    gen_tcp:send(Socket, <<?MISC:16, (length(Func)):32, 1:32, (length(Args)):32, ArgData/bytes>>),
    tyrant_response(Socket, fun recv_count_2tuple/2).    

ext(Socket, Func, Opts, Key, Value) when is_binary(Key), is_binary(Value) ->
    %% TODO: Opts needs to be parsed.  Probably as a proplist [record_lock, global_lock, neither...]
    gen_tcp:send(Socket, <<?EXT:16, (byte_size(Func)):32, Opts:32, (byte_size(Key)):32, (byte_size(Value)):32, Func/bytes, Key/bytes, Value/bytes>>),
    ?R_SUCCESS.

%%% What about the table api?
%%%
%%% See the Perl tyrant API.  All of the table stuff is done via misc().  Need to decide how the table
%%% records will be used internally: dict, record, other?


%% Handle response from the server
tyrant_response(Socket, ResponseHandler) ->
    receive
        {tcp, Socket, <<ErrorCode:8>>} when ErrorCode =/= 0 -> 
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
 
 
%%====================================================================
%% Generic handlers for common reply formats
%%====================================================================
 
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
 
