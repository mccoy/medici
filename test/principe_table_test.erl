-module(principe_table_test).

-export([test/0, test/1]).

test() ->
    test([]).

test(ConnectParams) ->
    TmpMod = principe:new(bad_val),
    {ok, Socket} = principe:connect(ConnectParams),
    case proplists:get_value(bigend, TmpMod:stat(Socket)) of
	"0" ->
	    Endian = little;
	"1" ->
	    Endian = big
    end,
    G = principe:new(Endian),
    Mod = principe_table:new(G),
    put_get_test(Mod).


put_get_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:put(Socket, <<"put_get1">>, <<"testval">>),
    ok = Mod:put(Socket, <<"put_get2">>, <<2#1101:32>>),
    <<"testval">> = Mod:get(Socket, <<"put_get1">>),
    <<2#1101:32>> = Mod:get(Socket, <<"put_get2">>),
    ok.
