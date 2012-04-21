-module(graphsom_folsom).

-include("graphsom.hrl").
 
-export([
         metric_values/2, 
         registered_metrics/0, 
         register/1, 
         deregister/1,
         register_type_handler/3,
         deregister_type_handler/1
        ]).

-spec registered_metrics() -> list().

registered_metrics() ->
    [ Metric || {Metric} <- ets:tab2list(?GRAPHSOM_FOLSOM_METRICS)].

-spec register(folsom_metric_name_type()) -> ok.

register(FolsomMetric) -> 
    true = ets:insert(?GRAPHSOM_FOLSOM_METRICS, {FolsomMetric}),
    ok.

-spec deregister(folsom_metric_name_type()) -> ok.

deregister(FolsomMetric) -> 
    true = ets:delete(?GRAPHSOM_FOLSOM_METRICS, FolsomMetric),
    ok.

-spec register_type_handler(folsom_metric_type(), atom(), atom()) -> ok.

register_type_handler(MetricType, Module, FunName) ->
    FolsomHandler = #graphsom_folsom_type_handler{ type = MetricType, 
                                                   module = Module, 
                                                   func = FunName },
    true = ets:insert(?GRAPHSOM_FOLSOM_TYPE_HANDLERS, FolsomHandler),
    ok.

-spec deregister_type_handler(folsom_metric_type()) -> ok.

deregister_type_handler(FolsomType) ->
    _ = ets:delete(?GRAPHSOM_FOLSOM_TYPE_HANDLERS, FolsomType),
    ok.

-spec metric_values(list(), list()) -> proplist().

metric_values(Metrics, VmMetrics) ->
    [{Metric, metric_value(Metric)} || Metric <- Metrics] ++
    [{VmMetric, vm_metric_value(VmMetric)} || VmMetric <- VmMetrics].

-spec vm_metric_value(folsom_vm_metric_type()) -> folsom_metric_value_type().

vm_metric_value(memory) -> folsom_vm_metrics:get_memory();
vm_metric_value(system_info) -> folsom_vm_metrics:get_system_info();
vm_metric_value(process_info) -> folsom_vm_metrics:get_process_info();
vm_metric_value(statistics) -> folsom_vm_metrics:get_statistics();
vm_metric_value(port_info) -> folsom_vm_metrics:get_port_info().

-spec metric_value(folsom_metric_name_type()) -> folsom_metric_value_type().

metric_value(MetricName) ->
    case catch folsom_metrics:get_metric_info(MetricName) of
        {'EXIT', _ } ->
            io:format("Metric ~s does not exist ~n", [MetricName]),
            [];
        [{MetricName, [{type, Type}]}] ->
            metric_value(MetricName, Type)
    end.

%% Special handling for histogram.
%% Export a generic handler for converting metrics to a propertylist

-spec metric_value(folsom_metric_name_type(), folsom_metric_type()) -> proplist().
 
metric_value(MetricName, MetricType) ->
    case ets:lookup(?GRAPHSOM_FOLSOM_TYPE_HANDLERS, MetricType) of
        [#graphsom_folsom_type_handler{ module = Module, func = Func }] ->
            handle_folsom_metric(MetricName, MetricType, Module, Func);
        _ ->
            folsom_metrics:get_metric_value(MetricName)
    end.

-spec handle_folsom_metric(folsom_metric_name_type(), folsom_metric_type(), atom(), atom()) -> list().
             
handle_folsom_metric(MetricName, MetricType, Module, Func) ->
    catch case erlang:apply(Module, Func, [MetricName, MetricType]) of
              {'EXIT', _Reason} ->
                  io:format("Unable to get value for metric using folsom handler: ~p, reason: ~w~n", [MetricName, _Reason]),
                  [];
              Val ->
                  Val
          end.
