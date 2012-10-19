-module(graphsom_metrics).

-include("graphsom.hrl").
 
-export([
        init/0,
        register/4,
        deregister/1,
        registered_metrics/0,
        all/0,
        all/2,
        default_folsom_handler/2
        ]).

-spec init() -> ok.

init() -> 
    create_tables_(),
    _ = [ graphsom_folsom:register_type_handler(Type, ?MODULE, default_folsom_handler) || Type <- [histogram, history]],
    ok.

-spec register(atom(), atom(), atom(), list()) -> ok.

register(MetricName, Module, FunName, Params) ->
    GMetric = #graphsom_metric{name = MetricName,  
                               module = Module, 
                               func = FunName,
                               params = Params},
    true = ets:insert(?GRAPHSOM_METRICS, GMetric),
    ok.

-spec deregister(atom()) -> ok.

deregister(MetricName) ->
    _ = ets:delete(?GRAPHSOM_METRICS, MetricName),
    ok.

-spec registered_metrics() -> list().

registered_metrics() ->
   [
    {graphsom, registered_metrics_()},
    {folsom, graphsom_folsom:registered_metrics()}
   ]. 

-spec all() -> proplist().

all() ->
    all([], false).

-spec all(list(), boolean()) -> proplist().

all(FolVmMetrics, AllFolMetrics) -> 
    graphsom_folsom:metric_values(folsom_metrics_(AllFolMetrics), FolVmMetrics) ++
    metric_values_().

%% Internal API

-spec folsom_metrics_(boolean()) -> list().

folsom_metrics_(true) -> folsom_metrics:get_metrics();
folsom_metrics_(_) -> graphsom_folsom:registered_metrics().

-spec metric_values_() -> proplist().

metric_values_() ->
    [metric_value_(Metric) || Metric  <- registered_metrics_()].

-spec metric_value_(graphsom_metric()) -> tuple().

metric_value_(#graphsom_metric{ name = Name, module = Module, func = Func, params = Params }) ->
    catch case erlang:apply(Module, Func, Params) of
              {'EXIT', _Reason} ->
                  error_logger:warning_msg("Unable to get value for metric: ~p, reason: ~w~n", [Name, _Reason]),
                  {Name, []};
              Val ->
                  {Name, Val}
          end.

-spec registered_metrics_() -> list().

registered_metrics_() ->
    [Metric || Metric <- ets:tab2list(?GRAPHSOM_METRICS)].

-spec create_tables_() -> ok.

create_tables_() ->
    _ = [create_table_(Name, KeyPos) || {Name, KeyPos} <- ?GRAPHSOM_ETS_TABLES],
    ok.

-spec create_table_(atom(), integer()) -> ok.

create_table_(Name, KeyPos) 
  when is_atom(Name),
       is_integer(KeyPos) ->
    _ = ets:new(Name, [set, named_table, public, {keypos, KeyPos}, {read_concurrency,true}]),
    ok.

-spec default_folsom_handler(folsom_metric_name_type(), folsom_metric_type()) -> list().

default_folsom_handler(Name, histogram) ->
    Values  = folsom_metrics:get_metric_value(Name),
    Stats = folsom_metrics:get_histogram_statistics(Name),
    [{count, length(Values)},
     {min, proplists:get_value(min, Stats)},
     {max, proplists:get_value(max, Stats)},
     {mean, proplists:get_value(arithmetic_mean, Stats)},
     {percentile, proplists:get_value(percentile, Stats)}
    ];
  
default_folsom_handler(Name, history) ->
    Values = folsom_metrics:get_metric_value(Name),
    [{count, length(Values)}].

    
