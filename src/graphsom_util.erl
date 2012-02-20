-module(graphsom_util).

-export([current_time/0, mean/1]).

-spec mean(list()) -> number().

mean(List) -> mean(List, 0, 0).

-spec mean(list(), non_neg_integer(), number()) -> number().

mean([H|T], C, S) -> mean(T, C + 1, S + H);
mean([], 0, _S) -> 0; 
mean([], C, S) -> S/C.

-spec current_time() -> pos_integer().

current_time() ->    
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600. 

