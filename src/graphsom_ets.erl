-module(graphsom_ets).

-include("graphsom.hrl").

-export([init/0]).

-spec init() -> ok.

init() -> create_tables_().
    
-spec create_tables_() -> ok.

create_tables_() ->
    _ = [create_table_(Name, KeyPos) || {Name, KeyPos} <- ?GRAPHSOM_ETS_TABLES],
    ok.

-spec create_table_(atom(), integer()) -> ok.

create_table_(Name, KeyPos) 
  when is_atom(Name),
       is_integer(KeyPos) ->
    _ = ets:new(Name, 
                [set, named_table, public, 
                 {keypos, KeyPos}, 
                 {read_concurrency,true}
                ]),
    ok.

