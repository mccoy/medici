-module(principe_table_test).

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
    G = principe:new(Endian),
    Mod = principe_table:new(G),
    put_get_test(Mod),
    putkeep_test(Mod),
    putcat_test(Mod),
    update_test(Mod),
    out_test(Mod),
    vsiz_test(Mod),
    vanish_test(Mod),
    addint_test(Mod),
    sync_test(Mod),
    size_test(Mod),
    rnum_test(Mod),
    stat_test(Mod),
    mget_test(Mod).

setup_column_data(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:vanish(Socket),
    ColData = [{"rec1", [{"name", "alice"}, {"sport", "baseball"}]},
	       {"rec2", [{"name", "bob"}, {"sport", "basketball"}]},
	       {"rec3", [{"name", "carol"}, {"age", "24"}]},
	       {"rec4", [{"name", "trent"}, {"age", "33"}, {"sport", "football"}]},
	       {"rec5", [{"name", "mallet"}, {"sport", "tennis"}, {"fruit", "apple"}]}
	       ],
    lists:foreach(fun({Key, ValProplist}) ->
			  Mod:put(Socket, Key, ValProplist)
		  end, ColData),
    Socket.

put_get_test(Mod) ->
    Socket = setup_column_data(Mod),
    [{<<"age">>, <<"24">>}, {<<"name">>, <<"carol">>}] = lists:sort(Mod:get(Socket, "rec3")),
    ok = Mod:put(Socket, <<"put_get1">>, [{"num", 32}]),
    case Mod of
	{_Principe, {_PrincipeTable, little}} ->
	     [{<<"num">>, <<32:32/little>>}] = lists:sort(Mod:get(Socket, <<"put_get1">>));
	{_Principe, {_PrincipeTable, big}} ->
	     [{<<"num">>, <<32:32>>}] = lists:sort(Mod:get(Socket, <<"put_get1">>))
    end,
    ok.

putkeep_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:vanish(Socket),
    ok = Mod:put(Socket, "putkeep1", [{"col1", "testval1"}]),
    [{<<"col1">>, <<"testval1">>}] = Mod:get(Socket, "putkeep1"),
    {error, _} = Mod:putkeep(Socket, <<"putkeep1">>, [{"col1", "testval2"}]),
    [{<<"col1">>, <<"testval1">>}] = Mod:get(Socket, "putkeep1"),
    ok = Mod:putkeep(Socket, <<"putkeep2">>, [{"col1", "testval2"}]),
    [{<<"col1">>, <<"testval2">>}] = Mod:get(Socket, "putkeep2"),
    ok.

putcat_test(Mod) ->
    Socket = setup_column_data(Mod),
    [{<<"age">>, <<"24">>}, 
     {<<"name">>, <<"carol">>}] = lists:sort(Mod:get(Socket, "rec3")),
    ok = Mod:putcat(Socket, "rec3", [{"sport", "golf"}]),
    [{<<"age">>, <<"24">>}, 
     {<<"name">>, <<"carol">>}, 
     {<<"sport">>, <<"golf">>}] = lists:sort(Mod:get(Socket, "rec3")),
    ok.

update_test(Mod) ->
    Socket = setup_column_data(Mod),
    [{<<"name">>, <<"alice">>},
     {<<"sport">>, <<"baseball">>}] = Mod:get(Socket, "rec1"),
    ok = Mod:update(Socket, "rec1", [{"sport", "swimming"}, {"pet", "dog"}]),
    [{<<"name">>, <<"alice">>},
     {<<"pet">>, <<"dog">>},
     {<<"sport">>, <<"swimming">>}] = lists:sort(Mod:get(Socket, "rec1")),
    ok.

out_test(Mod) ->
    Socket = setup_column_data(Mod),
    ok = Mod:out(Socket, <<"rec1">>),
    {error, _} = Mod:get(Socket, <<"rec1">>),
    ok.

vsiz_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:vanish(Socket),
    ColName = "col1",
    ColVal = "vsiz test",
    ok = Mod:put(Socket, "vsiz1", [{ColName, ColVal}]),
    ExpectedLength = length(ColName) + length(ColVal) + 2, % col + null sep + val + null column stop
    ExpectedLength = Mod:vsiz(Socket, "vsiz1"),
    ColName2 = "another col",
    ColVal2 = "more bytes",
    ok = Mod:put(Socket, "vsiz2", [{ColName, ColVal}, {ColName2, ColVal2}]),
    ExpectedLength2 = ExpectedLength + length(ColName2) + length(ColVal2) + 2,
    ExpectedLength2 = Mod:vsiz(Socket, "vsiz2"),
    ok.

vanish_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:vanish(Socket),
    ok = Mod:put(Socket, "vanish1", [{"col1", "going away"}]),
    ok = Mod:vanish(Socket),
    {error, _} = Mod:get(Socket, "vanish1"),
    ok.

addint_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:vanish(Socket),
    100 = Mod:addint(Socket, "addint1", 100),
    ok = Mod:put(Socket, "addint2", [{"_num", "10"}]), % see principe_table:addint edoc for why a string() is used
    20 = Mod:addint(Socket, "addint2", 10),
    [{<<"_num">>, <<"100">>}] = Mod:get(Socket, "addint1"),
    [{<<"_num">>, <<"20">>}] = Mod:get(Socket, "addint2"),
    ok.

sync_test(Mod) ->
    {ok, Socket} = Mod:connect(),
    ok = Mod:sync(Socket),
    ok.

rnum_test(Mod) ->
    Socket = setup_column_data(Mod),
    5 = Mod:rnum(Socket),
    ok = Mod:out(Socket, "rec1"),
    4 = Mod:rnum(Socket),
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

mget_test(Mod) ->
    Socket = setup_column_data(Mod),
    MGetData = Mod:mget(Socket, ["rec1", "rec3", "rec5"]),
    [{<<"name">>, <<"alice">>},{<<"sport">>, <<"baseball">>}] = lists:sort(proplists:get_value(<<"rec1">>, MGetData)),
    undefined = proplists:get_value(<<"rec2">>, MGetData),
    [<<"rec1">>, <<"rec3">>, <<"rec5">>] = lists:sort(proplists:get_keys(MGetData)),
    ok.

%% iter_test(Mod) ->
%%     {ok, Socket} = Mod:connect(),
%%     ok = Mod:vanish(Socket),
%%     ok = Mod:put(Socket, <<"a">>, <<"first">>),
%%     ok = Mod:iterinit(Socket),
%%     <<"a">> = Mod:iternext(Socket), % "a" should be the first key
%%     % Now to test a bit of real iteration
%%     ok = Mod:put(Socket, <<"b">>, <<"second">>),
%%     ok = Mod:put(Socket, <<"c">>, <<"third">>),
%%     ok = Mod:iterinit(Socket),
%%     One = Mod:iternext(Socket),
%%     Two = Mod:iternext(Socket),
%%     Three = Mod:iternext(Socket),
%%     {error, _} = Mod:iternext(Socket),
%%     [<<"a">>, <<"b">>, <<"c">>] = lists:sort([One, Two, Three]),
%%     ok.

%% fwmkeys_test(Mod) ->
%%     {ok, Socket} = Mod:connect(),
%%     ok = Mod:put(Socket, <<"fwmkeys1">>, <<"1">>),
%%     ok = Mod:put(Socket, <<"fwmkeys2">>, <<"2">>),
%%     ok = Mod:put(Socket, <<"fwmkeys3">>, <<"3">>),
%%     ok = Mod:put(Socket, <<"fwmkeys4">>, <<"4">>),
%%     Keys1 = Mod:fwmkeys(Socket, <<"fwmkeys">>, 4),
%%     4 = length(Keys1),
%%     true = lists:member(<<"fwmkeys1">>, Keys1),
%%     true = lists:member(<<"fwmkeys2">>, Keys1),
%%     true = lists:member(<<"fwmkeys3">>, Keys1),
%%     true = lists:member(<<"fwmkeys4">>, Keys1),
%%     Keys2 = Mod:fwmkeys(Socket, <<"fwmkeys">>, 2),
%%     2 = length(Keys2),
%%     ok.

%% TODO: All of the tests related to search() and its variants.
