-module(principe_table_test).

-export([test/0, test/1]).

-define(PRINT(Arg), io:format("~p~n", [Arg])).

test() ->
    test([]).

test(ConnectParams) ->
    put_get_test(ConnectParams),
    putkeep_test(ConnectParams),
    putcat_test(ConnectParams),
    update_test(ConnectParams),
    out_test(ConnectParams),
    vsiz_test(ConnectParams),
    vanish_test(ConnectParams),
    addint_test(ConnectParams),
    sync_test(ConnectParams),
    size_test(ConnectParams),
    rnum_test(ConnectParams),
    stat_test(ConnectParams),
    mget_test(ConnectParams),
    iter_test(ConnectParams),
    fwmkeys_test(ConnectParams),
    query_generation_test(),
    search_test(ConnectParams),
    searchcount_test(ConnectParams),
    searchout_test(ConnectParams).

setup_column_data(ConnectParams) ->
    {ok, Socket} = principe_table:connect(ConnectParams),
    ok = principe_table:vanish(Socket),
    ColData = [{"rec1", [{"name", "alice"}, {"sport", "baseball"}]},
	       {"rec2", [{"name", "bob"}, {"sport", "basketball"}]},
	       {"rec3", [{"name", "carol"}, {"age", "24"}]},
	       {"rec4", [{"name", "trent"}, {"age", "33"}, {"sport", "football"}]},
	       {"rec5", [{"name", "mallet"}, {"sport", "tennis"}, {"fruit", "apple"}]}
	       ],
    lists:foreach(fun({Key, ValProplist}) ->
			  ok = principe_table:put(Socket, Key, ValProplist)
		  end, ColData),
    Socket.

put_get_test(ConnectParams) ->
    Socket = setup_column_data(ConnectParams),
    [{<<"age">>, <<"24">>}, {<<"name">>, <<"carol">>}] = lists:sort(principe_table:get(Socket, "rec3")),
    ok = principe_table:put(Socket, <<"put_get1">>, [{"num", 32}]),
    % Note that by default integers go over to Tyrant in network byte-order
    [{<<"num">>, <<32:32>>}] = lists:sort(principe_table:get(Socket, <<"put_get1">>)),
    ok.

putkeep_test(ConnectParams) ->
    {ok, Socket} = principe_table:connect(ConnectParams),
    ok = principe_table:vanish(Socket),
    ok = principe_table:put(Socket, "putkeep1", [{"col1", "testval1"}]),
    [{<<"col1">>, <<"testval1">>}] = principe_table:get(Socket, "putkeep1"),
    {error, _} = principe_table:putkeep(Socket, <<"putkeep1">>, [{"col1", "testval2"}]),
    [{<<"col1">>, <<"testval1">>}] = principe_table:get(Socket, "putkeep1"),
    ok = principe_table:putkeep(Socket, <<"putkeep2">>, [{"col1", "testval2"}]),
    [{<<"col1">>, <<"testval2">>}] = principe_table:get(Socket, "putkeep2"),
    ok.

putcat_test(ConnectParams) ->
    Socket = setup_column_data(ConnectParams),
    [{<<"age">>, <<"24">>}, 
     {<<"name">>, <<"carol">>}] = lists:sort(principe_table:get(Socket, "rec3")),
    ok = principe_table:putcat(Socket, "rec3", [{"sport", "golf"}]),
    [{<<"age">>, <<"24">>}, 
     {<<"name">>, <<"carol">>}, 
     {<<"sport">>, <<"golf">>}] = lists:sort(principe_table:get(Socket, "rec3")),
    ok.

update_test(ConnectParams) ->
    Socket = setup_column_data(ConnectParams),
    [{<<"name">>, <<"alice">>},
     {<<"sport">>, <<"baseball">>}] = principe_table:get(Socket, "rec1"),
    ok = principe_table:update(Socket, "rec1", [{"sport", "swimming"}, {"pet", "dog"}]),
    [{<<"name">>, <<"alice">>},
     {<<"pet">>, <<"dog">>},
     {<<"sport">>, <<"swimming">>}] = lists:sort(principe_table:get(Socket, "rec1")),
    ok.

out_test(ConnectParams) ->
    Socket = setup_column_data(ConnectParams),
    ok = principe_table:out(Socket, <<"rec1">>),
    {error, _} = principe_table:get(Socket, <<"rec1">>),
    ok.

vsiz_test(ConnectParams) ->
    {ok, Socket} = principe_table:connect(ConnectParams),
    ok = principe_table:vanish(Socket),
    ColName = "col1",
    ColVal = "vsiz test",
    ok = principe_table:put(Socket, "vsiz1", [{ColName, ColVal}]),
    ExpectedLength = length(ColName) + length(ColVal) + 2, % col + null sep + val + null column stop
    ExpectedLength = principe_table:vsiz(Socket, "vsiz1"),
    ColName2 = "another col",
    ColVal2 = "more bytes",
    ok = principe_table:put(Socket, "vsiz2", [{ColName, ColVal}, {ColName2, ColVal2}]),
    ExpectedLength2 = ExpectedLength + length(ColName2) + length(ColVal2) + 2,
    ExpectedLength2 = principe_table:vsiz(Socket, "vsiz2"),
    ok.

vanish_test(ConnectParams) ->
    {ok, Socket} = principe_table:connect(ConnectParams),
    ok = principe_table:vanish(Socket),
    ok = principe_table:put(Socket, "vanish1", [{"col1", "going away"}]),
    ok = principe_table:vanish(Socket),
    {error, _} = principe_table:get(Socket, "vanish1"),
    ok.

addint_test(ConnectParams) ->
    {ok, Socket} = principe_table:connect(ConnectParams),
    ok = principe_table:vanish(Socket),
    100 = principe_table:addint(Socket, "addint1", 100),
    ok = principe_table:put(Socket, "addint2", [{"_num", "10"}]), % see principe_table:addint edoc for why a string() is used
    20 = principe_table:addint(Socket, "addint2", 10),
    [{<<"_num">>, <<"100">>}] = principe_table:get(Socket, "addint1"),
    [{<<"_num">>, <<"20">>}] = principe_table:get(Socket, "addint2"),
    ok.

sync_test(ConnectParams) ->
    {ok, Socket} = principe_table:connect(ConnectParams),
    ok = principe_table:sync(Socket),
    ok.

rnum_test(ConnectParams) ->
    Socket = setup_column_data(ConnectParams),
    5 = principe_table:rnum(Socket),
    ok = principe_table:out(Socket, "rec1"),
    4 = principe_table:rnum(Socket),
    ok = principe_table:vanish(Socket),
    0 = principe_table:rnum(Socket),
    ok.

size_test(ConnectParams) ->
    {ok, Socket} = principe_table:connect(ConnectParams),
    principe_table:size(Socket),
    ok.

stat_test(ConnectParams) ->
    {ok, Socket} = principe_table:connect(ConnectParams),
    principe_table:stat(Socket),
    ok.

mget_test(ConnectParams) ->
    Socket = setup_column_data(ConnectParams),
    MGetData = principe_table:mget(Socket, ["rec1", "rec3", "rec5"]),
    [{<<"name">>, <<"alice">>},{<<"sport">>, <<"baseball">>}] = lists:sort(proplists:get_value(<<"rec1">>, MGetData)),
    undefined = proplists:get_value(<<"rec2">>, MGetData),
    [<<"rec1">>, <<"rec3">>, <<"rec5">>] = lists:sort(proplists:get_keys(MGetData)),
    ok.

iter_test(ConnectParams) ->
    Socket = setup_column_data(ConnectParams),
    AllKeys = [<<"rec1">>, <<"rec2">>, <<"rec3">>, <<"rec4">>, <<"rec5">>],
    ok = principe_table:iterinit(Socket),
    First = principe_table:iternext(Socket),
    true = lists:member(First, AllKeys),
    IterAll = lists:foldl(fun(_Count, Acc) -> [principe_table:iternext(Socket) | Acc] end, 
			  [First], 
			  lists:seq(1, length(AllKeys)-1)),
    AllKeys = lists:sort(IterAll),
    {error, _} = principe_table:iternext(Socket),
    ok.

fwmkeys_test(ConnectParams) ->
    Socket = setup_column_data(ConnectParams),
    ok = principe_table:put(Socket, "fwmkeys1", [{"foo", "bar"}]),
    4 = length(principe_table:fwmkeys(Socket, "rec", 4)),
    5 = length(principe_table:fwmkeys(Socket, "rec", 8)),
    [<<"fwmkeys1">>] = principe_table:fwmkeys(Socket, "fwm", 3),
    [<<"rec1">>, <<"rec2">>, <<"rec3">>] = principe_table:fwmkeys(Socket, "rec", 3),
    ok.

query_generation_test() ->
    [{{set_order, primary, str_descending}, 
      ["setorder", <<0:8>>, <<0:8>>, <<0:8>>, "1"]}] = principe_table:query_order([], primary, str_descending),
    [{{set_order, "foo", str_ascending}, 
      ["setorder", <<0:8>>, "foo", <<0:8>>, "0"]}] = principe_table:query_order([{{set_order, blah}, ["foo"]}], "foo", str_ascending),
    [{{set_limit, 2, 0}, ["setlimit", <<0:8>>, "2", <<0:8>>, "0"]}] = principe_table:query_limit([], 2),
    [{{set_limit, 4, 1}, ["setlimit", <<0:8>>, "4", <<0:8>>, "1"]}] = principe_table:query_limit([{{set_limit, blah}, ["foo"]}], 4, 1),
    [{{add_cond, "foo", str_eq, ["bar"]}, 
      ["addcond", <<0:8>>, "foo", <<0:8>>, "0", <<0:8>>, ["bar"]]}] = principe_table:query_condition([], "foo", str_eq, ["bar"]),
    [{{add_cond, "foo", {no, str_and}, ["bar","baz"]},
     ["addcond", <<0:8>>, "foo", <<0:8>>, "16777220", <<0:8>>, ["bar",",","baz"]]}] = 
	principe_table:query_condition([], "foo", {no, str_and}, ["bar", "baz"]),
    ok.

search_test(ConnectParams) ->
    Socket = setup_column_data(ConnectParams),
    Query1 = principe_table:query_condition([], "name", str_eq, ["alice"]),
    [<<"rec1">>] = principe_table:search(Socket, Query1),
    Query2 = principe_table:query_condition([], "name", {no, str_eq}, ["alice"]),
    Query2A = principe_table:query_limit(Query2, 2),
    [<<"rec2">>, <<"rec3">>] = principe_table:search(Socket, Query2A),
    Query3 = principe_table:query_condition([], "age", num_ge, [25]),
    [<<"rec4">>] = principe_table:search(Socket, Query3),
    ok.

searchcount_test(ConnectParams) ->
    Socket = setup_column_data(ConnectParams),
    Query1 = principe_table:query_condition([], "name", str_or, ["alice", "bob"]),
    2 = principe_table:searchcount(Socket, Query1), 
    ok.

searchout_test(ConnectParams) ->
    Socket = setup_column_data(ConnectParams),
    5 = principe_table:rnum(Socket),
    %% Also testing regex matches, should hit "baseball" and "basketball" but
    %% skip "football"
    Query1 = principe_table:query_condition([], "sport", str_regex, ["^ba"]),
    ok = principe_table:searchout(Socket, Query1),
    3 = principe_table:rnum(Socket),
    {error, _} = principe_table:get(Socket, "rec1"),
    ok.
    
