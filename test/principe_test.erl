-module(principe_test).

%% Simple test-suite for principe
%% Would be nice if this could be an eunit test, but I can't seem to figure
%% out how to make eunit play nice with parameterized modules.
-export([test/0, test/1]).

test() ->
    test([]).

test(ConnectParams) ->
    TmpMod = principe:new(bad_val),
    {ok, Socket} = TmpMod:connect(ConnectParams),
    case proplists:get_value(bigend, TmpMod:stat(Socket)) of
	"0" ->
	    Endian = little;
	"1" ->
	    Endian = big
    end,
    Mod = principe:new(Endian),
    ok = put_get_test(Mod),
    ok = putkeep_test(Mod),
    ok = putcat_test(Mod),
    ok = putshl_test(Mod),
    ok = putnr_test(Mod),
    ok = out_test(Mod),
    ok = mget_test(Mod),
    ok = vsiz_test(Mod),
    ok = vanish_test(Mod),
    ok = iter_test(Mod),
    ok = fwmkeys_test(Mod),
    ok = addint_test(Mod),
    ok = sync_test(Mod),
    ok = rnum_test(Mod),
    ok = size_test(Mod),
    ok = stat_test(Mod),
    ok = misc_test(Mod),
    ok.


put_get_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:put(Socket, <<"put_get1">>, <<"testval">>),
    ok = Mod:put(Socket, <<"put_get2">>, <<2#1101:32>>),
    ok = Mod:put(Socket, <<"put_get3">>, 42),
    <<"testval">> = Mod:get(Socket, <<"put_get1">>),
    <<2#1101:32>> = Mod:get(Socket, <<"put_get2">>),
    42 = Mod:getint(Socket, <<"put_get3">>),
    % and just to make sure, do a raw check on the endianness of ints
    {_, Endian} = Mod,
    case Endian of
	little ->
	    <<42:32/little>> = Mod:get(Socket, <<"put_get3">>);
	big ->
	    <<42:32>> = Mod:get(Socket, <<"put_get3">>)
    end,
    ok.

putkeep_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:vanish(Socket),
    ok = Mod:put(Socket, <<"test">>, <<"foo">>),
    {error, _} = Mod:putkeep(Socket, <<"test">>, <<"bar">>),
    <<"foo">> = Mod:get(Socket, <<"test">>), % no effect if key already exists before putkeep
    ok = Mod:putkeep(Socket, <<"another">>, <<"baz">>),
    <<"baz">> = Mod:get(Socket, <<"another">>), % puts the key if key does not exist already
    ok.

putcat_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:vanish(Socket),
    ok = Mod:put(Socket, <<"putcat1">>, <<"foo">>),
    % append "bar" to the end
    ok = Mod:putcat(Socket, <<"putcat1">>, <<"bar">>),
    <<"foobar">> = Mod:get(Socket, <<"putcat1">>),
    ok.

putshl_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:vanish(Socket),
    ok = Mod:put(Socket, <<"putshl">>, <<"foo">>),
    % append "bar" to the end and shift to the left to retain the width of "4"
    ok = Mod:putshl(Socket, <<"putshl">>, <<"bar">>, 4),
    <<"obar">> = Mod:get(Socket, <<"putshl">>),
    ok.

putnr_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:vanish(Socket),
    Mod:putnr(Socket, <<"putnr1">>, <<"no reply">>),
    <<"no reply">> = Mod:get(Socket, <<"putnr1">>),
    ok.

out_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:vanish(Socket),
    ok = Mod:put(Socket, <<"out1">>, <<"to remove">>),
    <<"to remove">> = Mod:get(Socket, <<"out1">>),
    ok = Mod:out(Socket, <<"out1">>),
    {error, _} = Mod:get(Socket, <<"out1">>),
    ok.

mget_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:vanish(Socket),
    ok = Mod:put(Socket, <<"mget1">>, <<"alice">>),
    ok = Mod:put(Socket, <<"mget2">>, <<"bob">>),
    ok = Mod:put(Socket, <<"mget3">>, <<"carol">>),
    ok = Mod:put(Socket, <<"mget4">>, <<"trent">>),
    [{<<"mget1">>, <<"alice">>}, 
     {<<"mget2">>, <<"bob">>}, 
     {<<"mget3">>, <<"carol">>}, 
     {<<"mget4">>, <<"trent">>}] = Mod:mget(Socket, [<<"mget1">>, 
						     <<"mget2">>, 
						     <<"mget3">>, 
						     <<"mget4">>]),
    ok.

vsiz_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:vanish(Socket),
    ok = Mod:put(Socket, <<"vsiz1">>, <<"vsiz test">>),
    9 = Mod:vsiz(Socket, <<"vsiz1">>),
    ok.

vanish_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:vanish(Socket),
    ok = Mod:put(Socket, <<"vanish1">>, <<"going away">>),
    ok = Mod:vanish(Socket),
    {error, _} = Mod:get(Socket, <<"vanish1">>),
    ok.

iter_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:vanish(Socket),
    ok = Mod:put(Socket, <<"a">>, <<"first">>),
    ok = Mod:iterinit(Socket),
    <<"a">> = Mod:iternext(Socket), % "a" should be the first key
    % Now to test a bit of real iteration
    ok = Mod:put(Socket, <<"b">>, <<"second">>),
    ok = Mod:put(Socket, <<"c">>, <<"third">>),
    ok = Mod:iterinit(Socket),
    One = Mod:iternext(Socket),
    Two = Mod:iternext(Socket),
    Three = Mod:iternext(Socket),
    {error, _} = Mod:iternext(Socket),
    [<<"a">>, <<"b">>, <<"c">>] = lists:sort([One, Two, Three]),
    ok.

fwmkeys_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:put(Socket, <<"fwmkeys1">>, <<"1">>),
    ok = Mod:put(Socket, <<"fwmkeys2">>, <<"2">>),
    ok = Mod:put(Socket, <<"fwmkeys3">>, <<"3">>),
    ok = Mod:put(Socket, <<"fwmkeys4">>, <<"4">>),
    Keys1 = Mod:fwmkeys(Socket, <<"fwmkeys">>, 4),
    4 = length(Keys1),
    true = lists:member(<<"fwmkeys1">>, Keys1),
    true = lists:member(<<"fwmkeys2">>, Keys1),
    true = lists:member(<<"fwmkeys3">>, Keys1),
    true = lists:member(<<"fwmkeys4">>, Keys1),
    Keys2 = Mod:fwmkeys(Socket, <<"fwmkeys">>, 2),
    2 = length(Keys2),
    ok.

addint_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:put(Socket, <<"addint1">>, 100),
    100 = Mod:getint(Socket, <<"addint1">>),
    120 = Mod:addint(Socket, <<"addint1">>, 20),
    120 = Mod:getint(Socket, <<"addint1">>),
    ok.

sync_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:sync(Socket),
    ok.

rnum_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:vanish(Socket),
    ok = Mod:put(Socket, <<"rnum1">>, <<"foo">>),
    ok = Mod:put(Socket, <<"rnum2">>, <<"foo">>),
    2 = Mod:rnum(Socket),
    ok = Mod:vanish(Socket),
    0 = Mod:rnum(Socket),
    ok.

size_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    Mod:size(Socket),
    ok.

stat_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    Mod:stat(Socket),
    ok.

misc_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:vanish(Socket),
    [] = Mod:misc(Socket, "putlist",
		       ["key1", "value1",
			"key2", "value2",
			"key3", "value3",
			"key4", "value4"]),
    4 = Mod:rnum(Socket),
    <<"value1">> = Mod:get(Socket, "key1"),
    [] = Mod:misc(Socket, "outlist",
		       ["key1", "key2", "key3"]),
    1 = Mod:rnum(Socket),
    Mod:put(Socket, "key5", "value5"),
    GetlistOut = Mod:misc(Socket, "getlist", ["key4", "key5"]),
    true = lists:all(fun (K) -> lists:member(K, GetlistOut) end, 
		     [<<"key4">>, <<"value4">>, 
		      <<"key5">>, <<"value5">>]),
    ok.
