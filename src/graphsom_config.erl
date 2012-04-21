-module(graphsom_config).

-include("graphsom.hrl").
-include("graphsom_config.hrl").

-export([init/0, get_config/1, report_to/0]).

%% PUBLIC API %%%%%%

-spec init() -> ok.

init() ->
    AppConf = application:get_all_env(graphsom),
    %% extract and save application-wide graphsom config
    save_config(graphsom, graphsom_config(AppConf)),
    %% save config for each visualization backend
    [save_config(B, config(B, AppConf)) || B <- ?GRAPHSOM_BACKENDS].
    
-spec get_config(atom()) -> proplist().

get_config(B) ->
    case ets:lookup(?GRAPHSOM_CONFIGS, B) of
        [{B, Config}] ->
            Config;
        _  ->
            []
    end.

-spec report_to() -> list().

report_to() ->
    lookup(report_to, ?GRAPHSOM_CONFIGS, []).
    
-spec lookup(atom(), atom(), term()) -> term().

lookup(Key, Tab, Default) ->
    case ets:lookup(Tab, report_to) of
        [Config] ->
            proplists:get_value(Key, Config, Default);
        _ ->
            Default
    end.

-spec save_config(atom(), proplist()) -> ok.

save_config(B, Conf) ->
    true = ets:insert(?GRAPHSOM_CONFIGS, {B, Conf}),
    ok.

-spec graphsom_config(proplist()) -> proplist().

graphsom_config(AppConfig) ->
    [{report_to, proplists:get_value(report_to, AppConfig, [])}].

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

