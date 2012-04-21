-module(graphsom_config).

-include("graphsom.hrl").
-include("graphsom_config.hrl").

-export([
         init/0, 
         get_config/1, 
         report_to/0,
         min_interval/0,
         lookup_key/3
        ]).

%% PUBLIC API %%%%%%

-spec init() -> ok.

init() ->
    AppConf = application:get_all_env(graphsom),
    %% extract and save application-wide graphsom config
    save_config(?GRAPHSOM_CONFIG_KEY, graphsom_config(AppConf)),
    %% save config for each visualization backend
    [save_config(Key, config(Key, AppConf)) || Key <- ?GRAPHSOM_BACKENDS].
    
-spec report_to() -> list().

report_to() ->
    lookup_key(report_to, ?GRAPHSOM_CONFIG_KEY, []).

-spec min_interval() -> number() | undefined.

min_interval() ->
    L = [lookup_key(report_interval, K, 0) || K <- report_to()],
    case L of
        [] ->
            undefined;
        Ivls ->
            lists:min(Ivls)
    end.
    
-spec lookup_key(atom(), atom(), term()) -> term().

lookup_key(Key, ConfKey, Default) 
  when is_atom(Key), 
       is_atom(ConfKey)->
    proplists:get_value(Key, get_config(ConfKey), Default).

-spec get_config(atom()) -> proplist().

get_config(ConfKey) ->
    case ets:lookup(?GRAPHSOM_CONFIGS, ConfKey) of
        [{ConfKey, Config}] ->
            Config;
        _  ->
            []
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

