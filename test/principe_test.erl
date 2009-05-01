%% Simple test-suite for principe

put_get_test() ->
    {ok, Socket} = principe:connect(),
    ok = principe:put(Socket, <<"put_get1">>, <<"testval">>),
    ok = principe:put(Socket, <<"put_get2">>, <<2#1101:32>>),
    <<"testval">> = principe:get(Socket, <<"put_get1">>),
    <<2#1101:32>> = principe:get(Socket, <<"put_get2">>),
    ok.

putkeep_test() ->
    {ok, Socket} = principe:connect(),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"test">>, <<"foo">>),
    {error, _} = principe:putkeep(Socket, <<"test">>, <<"bar">>),
    <<"foo">> = principe:get(Socket, <<"test">>), % no effect if key already exists before putkeep
    ok = principe:putkeep(Socket, <<"another">>, <<"baz">>),
    <<"baz">> = principe:get(Socket, <<"another">>), % puts the key if key does not exist already
    ok.

putcat_test() ->
    {ok, Socket} = principe:connect(),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"putcat1">>, <<"foo">>),
    % append "bar" to the end
    ok = principe:putcat(Socket, <<"putcat1">>, <<"bar">>),
    <<"foobar">> = principe:get(Socket, <<"putcat1">>),
    ok.

putsh1_test() ->
    {ok, Socket} = principe:connect(),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"putshl">>, <<"foo">>),
    % append "bar" to the end and shift to the left to retain the width of "4"
    ok = principe:putshl(Socket, <<"putshl">>, <<"bar">>, 4),
    <<"obar">> = principe:get(Socket, <<"putshl">>),
    ok.

putnr_test() ->
    {ok, Socket} = principe:connect(),
    ok = principe:vanish(Socket),
    principe:putnr(Socket, <<"putnr1">>, <<"mango">>),
    <<"mango">> = principe:get(Socket, <<"putnr1">>),
    ok.

out_test() ->
    {ok, Socket} = principe:connect(),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"out1">>, <<"germany">>),
    <<"germany">> = principe:get(Socket, <<"out1">>),
    ok = principe:out(Socket, <<"out1">>),
    {error, _} = principe:get(Socket, <<"out1">>),
    ok.

mget_test() ->
    {ok, Socket} = principe:connect(),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"mget1">>, <<"usa">>),
    ok = principe:put(Socket, <<"mget2">>, <<"canada">>),
    ok = principe:put(Socket, <<"mget3">>, <<"singapore">>),
    ok = principe:put(Socket, <<"mget4">>, <<"india">>),
    [{<<"mget1">>, <<"usa">>}, {<<"mget2">>, <<"canada">>}, 
        {<<"mget3">>, <<"singapore">>}, {<<"mget4">>, <<"india">>} ] = principe:mget(Socket, [<<"mget1">>, <<"mget2">>, <<"mget3">>, <<"mget4">>]),
    ok.

vsiz_test() ->
    {ok, Socket} = principe:connect(),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"vsiz1">>, <<"singapore">>),
    9 = principe:vsiz(Socket, <<"vsiz1">>),
    ok.

vanish_test() ->
    {ok, Socket} = principe:connect(),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"vanish1">>, <<"what a waste">>),
    ok = principe:vanish(Socket),
    {error, _} = principe:get(Socket, <<"vanish1">>),
    ok.

iter_test() ->
    {ok, Socket} = principe:connect(),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"a">>, <<"first">>),
    ok = principe:iterinit(Socket),
    <<"a">> = principe:iternext(Socket), % "a" should be the first key
    ok.

fwmkeys_test() ->
    {ok, Socket} = principe:connect(),
    ok = principe:put(Socket, <<"fwmkeys1">>, <<"1">>),
    ok = principe:put(Socket, <<"fwmkeys2">>, <<"2">>),
    ok = principe:put(Socket, <<"fwmkeys3">>, <<"3">>),
    ok = principe:put(Socket, <<"fwmkeys4">>, <<"4">>),

    Keys1 = principe:fwmkeys(Socket, <<"fwmkeys">>, 4),
    ?assertEqual(4, length(Keys1)),
    ?assert(lists:member(<<"fwmkeys1">>, Keys1)),
    ?assert(lists:member(<<"fwmkeys2">>, Keys1)),
    ?assert(lists:member(<<"fwmkeys3">>, Keys1)),
    ?assert(lists:member(<<"fwmkeys4">>, Keys1)),

    Keys2 = principe:fwmkeys(Socket, <<"fwmkeys">>, 2),
    ?assertEqual(2, length(Keys2)),
    ok.

addint_test() ->
    {ok, Socket} = principe:connect(),
    ok = principe:put(Socket, <<"addint1">>, <<100:32/little>>),
    <<100:32/little>> = principe:get(Socket, <<"addint1">>),
    120 = principe:addint(Socket, <<"addint1">>, 20),
    ok.

sync_test() ->
    {ok, Socket} = principe:connect(),
    ok = principe:sync(Socket),
    ok.



rnum_test() ->
    {ok, Socket} = principe:connect(),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"rnum1">>, <<"foo">>),
    ok = principe:put(Socket, <<"rnum2">>, <<"foo">>),
    ?assertEqual(2, principe:rnum(Socket)),
    ok = principe:vanish(Socket),
    ?assertEqual(0, principe:rnum(Socket)),
    ok.

size_test() ->
    {ok, Socket} = principe:connect(),
    principe:size(Socket),
    ok.

stat_test() ->
    {ok, Socket} = principe:connect(),
    principe:stat(Socket),
    ok.

misc_test() ->
    {ok, Socket} = principe:connect(),
    ok = principe:vanish(Socket),
    [] = principe:misc(Socket, "putlist",
		       ["key1", "value1",
			"key2", "value2",
			"key3", "value3",
			"key4", "value4"]),
    4 = principe:rnum(Socket),
    <<"value1">> = principe:get(Socket, "key1"),
    [] = principe:misc(Socket, "outlist",
		       ["key1", "key2", "key3"]),
    1 = principe:rnum(Socket),
    principe:put(Socket, "key5", "value5"),
    GetlistOut = principe:misc(Socket, "getlist", ["key4", "key5"]),
    true = lists:all(fun (K) -> lists:member(K, GetlistOut) end, 
		     [<<"key4">>, <<"value4">>, 
		      <<"key5">>, <<"value5">>]),
    ok.

