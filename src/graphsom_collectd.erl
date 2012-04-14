-module(graphsom_collectd).

-include("graphsom.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([stringify_proplist_metric/6]).

%% The core function to convert a proplist metric to string representation for collectd.
%% The plain text based collectd protocol for PUTVAL is used as specified at
%% http://collectd.org/wiki/index.php/Plain_text_protocol#PUTVAL
%% PUTVAL Identifier [OptionList] Valuelist

-spec stringify_proplist_metric(folsom_metric_name_type(), folsom_metric_value_type(), string(), pos_integer(), number(), string()) -> string().

stringify_proplist_metric(Name, Value, Prefix, Interval, Time, Acc) 
  when is_number(Value),
       is_integer(Interval) ->
    lists:flatten(Acc ++ io_lib:format("PUTVAL ~s/~s interval=~w ~w:~w~n", [Prefix, Name, Interval, Time, Value]));

stringify_proplist_metric(Name, {SubName, Value}, Prefix, Interval, Time, Acc) when is_integer(Interval) ->
    stringify_proplist_metric(SubName, Value, io_lib:format("~s/~s", [Prefix, Name]), Interval, Time, Acc);

stringify_proplist_metric(Name, L = [H | T], Prefix, Interval, Time, Acc) ->
    case graphsom_util:is_num_list(L) of
        true ->
            VStr = [io_lib:format(":~w", [V]) || V <- L] ++ "\n",
            lists:flatten(Acc ++ io_lib:format("PUTVAL ~s/~s interval=~w ~w", [Prefix, Name, Interval, Time]) ++ VStr);
        _ ->        
            Acc1 = stringify_proplist_metric(Name, H, Prefix, Interval, Time, Acc),
            stringify_proplist_metric(Name, T, Prefix, Interval, Time, Acc1)
    end;

stringify_proplist_metric(_Name, _Value, _Prefix, _Interval, _Time, Acc) ->
    Acc.

%%%%% UNIT TESTS  %%%%%%%

-spec stringify_single_value_test() -> ok.

stringify_single_value_test() ->
    Expected = "PUTVAL node-11/myapp/metric_1 interval=60 123456:40.123\n",
    Actual = stringify_proplist_metric("metric_1", 40.123, "node-11/myapp", 60, 123456, ""),
    ?assertEqual(Expected, Actual).

-spec stringify_num_list_test() -> ok.

stringify_num_list_test() ->
    Expected = "PUTVAL node-11/myapp/metric_1 interval=60 123456:0:20:31:10:42.1233:60.0:100:0.0\n",
    Values = [0, 20, 31, 10, 42.1233, 60.0, 100, 0.0],
    Actual = stringify_proplist_metric("metric_1", Values, "node-11/myapp", 60, 123456, ""),
    ?assertEqual(Expected, Actual).

-spec stringify_simple_proplist_test() -> ok.

stringify_simple_proplist_test() ->
    L1 = "PUTVAL node-11/myapp/metric_1/count interval=60 123456:100\n",
    L2 = "PUTVAL node-11/myapp/metric_1/mean interval=60 123456:1\n",
    L3 = "PUTVAL node-11/myapp/metric_1/one interval=60 123456:2.11\n",
    L4 = "PUTVAL node-11/myapp/metric_1/five interval=60 123456:1.0\n",
    L5 = "PUTVAL node-11/myapp/metric_1/fifteen interval=60 123456:3.1234\n",
    Expected = L1 ++ L2 ++ L3 ++ L4 ++ L5,
    Values = [{count, 100}, 
              {mean, 1}, 
              {one, 2.11},
              {five, 1.0},
              {fifteen, 3.1234}],
    Actual = stringify_proplist_metric("metric_1", Values, "node-11/myapp", 60, 123456, ""),
    ?assertEqual(Expected, Actual).
    
-spec stringify_nested_proplist_test() -> ok.

stringify_nested_proplist_test() ->
    L1 = "PUTVAL node-11/myapp/metric_1/count interval=60 123456:47\n",
    L2 = "PUTVAL node-11/myapp/metric_1/one interval=60 123456:0.4413761384322548\n",
    L3 = "PUTVAL node-11/myapp/metric_1/five interval=60 123456:0.0975321200376695\n",
    L4 = "PUTVAL node-11/myapp/metric_1/fifteen interval=60 123456:0.03305675226584654\n",
    L5 = "PUTVAL node-11/myapp/metric_1/mean interval=60 123456:8.617708389619651e-7\n",
    L6 = "PUTVAL node-11/myapp/metric_1/acceleration/one_to_five interval=60 123456:0.0011461467279819512\n",
    L7 = "PUTVAL node-11/myapp/metric_1/acceleration/five_to_fifteen interval=60 123456:1.0745894628637162e-4\n",
    L8 = "PUTVAL node-11/myapp/metric_1/acceleration/random_vals interval=60 123456:10:20:30:40\n",
    Expected = L1 ++ L2 ++ L3 ++ L4 ++ L5 ++ L6 ++ L7 ++ L8,
    Values =  [{count,47},
               {one,0.4413761384322548},
               {five,0.0975321200376695},
               {fifteen,0.03305675226584654},
               {mean,8.617708389619651e-7},
               {acceleration,[{one_to_five,0.0011461467279819512},
                              {five_to_fifteen,1.0745894628637162e-4},
                              {random_vals, [10, 20, 30, 40]}]}],
    Actual = stringify_proplist_metric("metric_1", Values, "node-11/myapp", 60, 123456, ""),
    ?assertEqual(Expected, Actual).

-spec test() -> ok.
