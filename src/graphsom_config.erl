-module(graphsom_config).

-include("graphsom.hrl").
-include("graphsom_config.hrl").

-export([init/0, get_config/1]).

-spec init() -> ok.

init() ->
    AppConf = application:get_all_env(graphsom), 
    [save_config(B, config(B, AppConf)) || B <- ?GRAPHSOM_BACKENDS].
    
-spec get_config(atom()) -> proplist().

get_config(B) ->
    case ets:lookup(?GRAPHSOM_CONFIGS, B) of
        [{B, Config}] ->
            Config;
        _  ->
            []
    end.
    
-spec save_config(atom(), proplist()) -> ok.

save_config(B, Conf) ->
    true = ets:insert(?GRAPHSOM_CONFIGS, {B, Conf}),
    ok.

-spec config(atom(), proplist()) -> proplist().

config(Backend, AppConf) ->
    UserConf = proplists:get_value(Backend, AppConf, []), 
    DefaultConf = default_config(Backend),
    merge_config(UserConf, DefaultConf, []).
    
-spec merge_config(proplist(), proplist(), proplist()) -> proplist().

merge_config([],[], Acc) ->
    Acc;
merge_config([H|R], D, Acc) ->
    merge_config(R, D, Acc ++ H);
merge_config([], [H|R], Acc) ->
    merge_config([], R, Acc ++ H).

-spec default_config(atom()) -> proplist().

default_config(graphite) -> ?GRAPHSOM_GRAPHITE;
default_config(collectd) -> ?GRAPHSOM_COLLECTD; 
default_config(_) -> []. 

%% UNIT TESTS %%%%%%
