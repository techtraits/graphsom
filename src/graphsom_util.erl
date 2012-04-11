-module(graphsom_util).

-include_lib("eunit/include/eunit.hrl").

-export([current_time/0, mean/1, is_num_list/1]).

-spec mean(list()) -> number().

mean(List) -> mean(List, 0, 0).

-spec mean(list(), non_neg_integer(), number()) -> number().

mean([H|T], C, S) -> mean(T, C + 1, S + H);
mean([], 0, _S) -> 0; 
mean([], C, S) -> S/C.

-spec current_time() -> pos_integer().

current_time() ->    
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600. 

-spec is_num_list(list()) -> boolean().

is_num_list([]) -> true;
is_num_list([H|T]) when is_number(H) -> is_num_list(T);
is_num_list(_) -> false.

%% UNIT TESTS %%

-spec is_num_list_test() -> ok.

is_num_list_test() ->
    ?assertEqual(true, is_num_list([])),
    ?assertEqual(true, is_num_list([123.2])),
    ?assertEqual(true, is_num_list([1, 2, 3, 5 ,9])),
    ?assertEqual(true, is_num_list([12, 0.23])),
    ?assertEqual(false, is_num_list([23, [767], 90])),
    ?assertEqual(false, is_num_list([120, {count, 123}])),
    ?assertEqual(false, is_num_list(405)),
    ?assertEqual(false, is_num_list({23, 65})).

-spec test() -> ok.

