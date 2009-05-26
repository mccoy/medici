-module(principe_test).

%% Simple test-suite for principe
%% Would be nice if this could be an eunit test, but I can't seem to figure
%% out how to make eunit play nice with parameterized modules.
-export([test/0, test/1]).

test() ->
    test([]).

test(ConnectParams) ->
    ok = put_get_test(ConnectParams),
    ok = put_get_random_test(ConnectParams, 1024),
    ok = putkeep_test(ConnectParams),
    ok = putcat_test(ConnectParams),
    ok = putshl_test(ConnectParams),
    ok = putnr_test(ConnectParams),
    ok = out_test(ConnectParams),
    ok = mget_test(ConnectParams),
    ok = vsiz_test(ConnectParams),
    ok = vanish_test(ConnectParams),
    ok = iter_test(ConnectParams),
    ok = fwmkeys_test(ConnectParams),
    ok = addint_test(ConnectParams),
    ok = sync_test(ConnectParams),
    ok = rnum_test(ConnectParams),
    ok = size_test(ConnectParams),
    ok = stat_test(ConnectParams),
    ok = misc_test(ConnectParams),
    ok.


put_get_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
    ok = principe:put(Socket, "put_get1", "testval"),
    ok = principe:put(Socket, <<"put_get2">>, <<32,145,56,0,14>>),
    <<"testval">> = principe:get(Socket, <<"put_get1">>),
    <<32, 145, 56, 0, 14>> = principe:get(Socket, "put_get2"),
    case proplists:get_value(bigend, principe:stat(Socket)) of
	"0" ->
	    ok = principe:put(Socket, <<"put_get3">>, 42, little),
	    <<42:32/little>> = principe:get(Socket, <<"put_get3">>);
	"1" ->
	    ok = principe:put(Socket, <<"put_get3">>, 42, big),
	    <<42:32>> = principe:get(Socket, <<"put_get3">>)
    end,
    ok.

put_get_random_test(ConnectParams, ElementCount) ->
    crypto:start(),
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    {ok, Socket} = principe:connect(ConnectParams),
    PutVals = lists:foldl(fun(_Seq, Acc) ->
				  KeySize = random:uniform(1024),
				  Key = crypto:rand_bytes(KeySize),
				  ValSize = random:uniform(65536),
				  Val = crypto:rand_bytes(ValSize),
				  principe:put(Socket, Key, Val),
				  [{Key, Val} | Acc]
			  end, [], lists:seq(1, ElementCount)),
    lists:foreach(fun({K, V}) ->
			  V = principe:get(Socket, K)
		  end, PutVals),
    ok.

putkeep_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"test">>, <<"foo">>),
    {error, _} = principe:putkeep(Socket, <<"test">>, <<"bar">>),
    <<"foo">> = principe:get(Socket, <<"test">>), % no effect if key already exists before putkeep
    ok = principe:putkeep(Socket, <<"another">>, <<"baz">>),
    <<"baz">> = principe:get(Socket, <<"another">>), % puts the key if key does not exist already
    ok.

putcat_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"putcat1">>, <<"foo">>),
    % append "bar" to the end
    ok = principe:putcat(Socket, <<"putcat1">>, <<"bar">>),
    <<"foobar">> = principe:get(Socket, <<"putcat1">>),
    ok.

putshl_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"putshl">>, <<"foo">>),
    % append "bar" to the end and shift to the left to retain the width of "4"
    ok = principe:putshl(Socket, <<"putshl">>, <<"bar">>, 4),
    <<"obar">> = principe:get(Socket, <<"putshl">>),
    ok.

putnr_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
    ok = principe:vanish(Socket),
    principe:putnr(Socket, <<"putnr1">>, <<"no reply">>),
    <<"no reply">> = principe:get(Socket, <<"putnr1">>),
    ok.

out_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"out1">>, <<"to remove">>),
    <<"to remove">> = principe:get(Socket, <<"out1">>),
    ok = principe:out(Socket, <<"out1">>),
    {error, _} = principe:get(Socket, <<"out1">>),
    ok.

mget_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"mget1">>, <<"alice">>),
    ok = principe:put(Socket, <<"mget2">>, <<"bob">>),
    ok = principe:put(Socket, <<"mget3">>, <<"carol">>),
    ok = principe:put(Socket, <<"mget4">>, <<"trent">>),
    [{<<"mget1">>, <<"alice">>}, 
     {<<"mget2">>, <<"bob">>}, 
     {<<"mget3">>, <<"carol">>}, 
     {<<"mget4">>, <<"trent">>}] = principe:mget(Socket, [<<"mget1">>, 
						     <<"mget2">>, 
						     <<"mget3">>, 
						     <<"mget4">>]),
    ok.

vsiz_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"vsiz1">>, <<"vsiz test">>),
    9 = principe:vsiz(Socket, <<"vsiz1">>),
    ok.

vanish_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"vanish1">>, <<"going away">>),
    ok = principe:vanish(Socket),
    {error, _} = principe:get(Socket, <<"vanish1">>),
    ok.

iter_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"a">>, <<"first">>),
    ok = principe:iterinit(Socket),
    <<"a">> = principe:iternext(Socket), % "a" should be the first key
    % Now to test a bit of real iteration
    ok = principe:put(Socket, <<"b">>, <<"second">>),
    ok = principe:put(Socket, <<"c">>, <<"third">>),
    ok = principe:iterinit(Socket),
    One = principe:iternext(Socket),
    Two = principe:iternext(Socket),
    Three = principe:iternext(Socket),
    {error, _} = principe:iternext(Socket),
    [<<"a">>, <<"b">>, <<"c">>] = lists:sort([One, Two, Three]),
    ok.

fwmkeys_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
    ok = principe:put(Socket, <<"fwmkeys1">>, <<"1">>),
    ok = principe:put(Socket, <<"fwmkeys2">>, <<"2">>),
    ok = principe:put(Socket, <<"fwmkeys3">>, <<"3">>),
    ok = principe:put(Socket, <<"fwmkeys4">>, <<"4">>),
    Keys1 = principe:fwmkeys(Socket, <<"fwmkeys">>, 4),
    4 = length(Keys1),
    true = lists:member(<<"fwmkeys1">>, Keys1),
    true = lists:member(<<"fwmkeys2">>, Keys1),
    true = lists:member(<<"fwmkeys3">>, Keys1),
    true = lists:member(<<"fwmkeys4">>, Keys1),
    Keys2 = principe:fwmkeys(Socket, <<"fwmkeys">>, 2),
    2 = length(Keys2),
    ok.

addint_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
    case proplists:get_value(bigend, principe:stat(Socket)) of
	"0" ->
	    principe:put(Socket, <<"addint1">>, 100, little);
	"1" ->
	    principe:put(Socket, <<"addint1">>, 100)
    end,
    120 = principe:addint(Socket, <<"addint1">>, 20),
    ok.

sync_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
    ok = principe:sync(Socket),
    ok.

rnum_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
    ok = principe:vanish(Socket),
    ok = principe:put(Socket, <<"rnum1">>, <<"foo">>),
    ok = principe:put(Socket, <<"rnum2">>, <<"foo">>),
    2 = principe:rnum(Socket),
    ok = principe:vanish(Socket),
    0 = principe:rnum(Socket),
    ok.

size_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
    principe:size(Socket),
    ok.

stat_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
    principe:stat(Socket),
    ok.

misc_test(ConnectParams) ->
    {ok, Socket} = principe:connect(ConnectParams),
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
