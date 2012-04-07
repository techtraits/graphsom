-module(graphsom).

%% all graphsom api would go in here
-export([register_folsom_metric/1,
         deregister_folsom_metric/1,
         register_graphsom_metric/3,
         register_graphsom_metric/4,
         deregister_graphsom_metric/1,
         registered_metrics/0,
         report_now/0,
         start_reporting/0,
         stop_reporting/0,
         update_config/2]).

%% Includes

-include("graphsom.hrl").

%% API

-spec register_folsom_metric(folsom_metric_name_type()) -> ok | {error, term()}.

register_folsom_metric(FolsomMetric) ->
   graphsom_folsom:register(FolsomMetric).

-spec deregister_folsom_metric(folsom_metric_name_type()) -> ok | {error, term()}.

deregister_folsom_metric(FolsomMetric) ->
   graphsom_folsom:deregister(FolsomMetric).

-spec register_graphsom_metric(atom(), atom(), atom()) -> ok | {error, term()}.

register_graphsom_metric(Name, Module, Func) ->
    graphsom_metrics:register(Name, Module, Func, []).

-spec register_graphsom_metric(atom(), atom(), atom(), list()) -> ok | {error, term()}.

register_graphsom_metric(Name, Module, Func, Params) when is_list(Params) ->
    graphsom_metrics:register(Name, Module, Func, Params).

-spec deregister_graphsom_metric(atom()) -> ok.

deregister_graphsom_metric(Name) ->
    graphsom_metrics:deregister(Name).

-spec registered_metrics() -> proplist().

registered_metrics() ->
   graphsom_metrics:registered_metrics().

-spec report_now() -> ok | {error, term()}.

report_now() ->
    graphsom_timer:report_now().

-spec start_reporting() -> ok | {error, term()}.

start_reporting() ->
   graphsom_timer:start_reporting().

-spec stop_reporting() -> ok | {error, term()}.

stop_reporting() ->
   graphsom_timer:stop_reporting().

-spec update_config(atom(), term()) -> ok.

update_config(Key, Val) ->
    graphsom_timer:update_config(Key, Val).
