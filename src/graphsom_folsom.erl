-module(graphsom_folsom).

-include("graphsom.hrl").
 
-export([get_metrics/2]).

-spec get_metrics(list(), list()) -> proplist().

get_metrics(Metrics, VmMetrics) ->
    [{Metric, get_metric_value(Metric)} || Metric <- Metrics] ++
    [{VmMetric, get_vm_metric_value(VmMetric)} || VmMetric <- VmMetrics].

-spec get_vm_metric_value(folsom_vm_metric_type()) -> folsom_metric_value_type().

get_vm_metric_value(memory) -> folsom_vm_metrics:get_memory();
get_vm_metric_value(system_info) -> folsom_vm_metrics:get_system_info();
get_vm_metric_value(process_info) -> folsom_vm_metrics:get_process_info();
get_vm_metric_value(statistics) -> folsom_vm_metrics:get_statistics();
get_vm_metric_value(port_info) -> folsom_vm_metrics:get_port_info().

-spec get_metric_value(folsom_metric_name_type()) -> folsom_metric_value_type().

get_metric_value(MetricName) ->
    case catch folsom_metrics:get_metric_info(MetricName) of
        {'EXIT', _ } ->
            io:format("Metric ~s does not exist ~n", [MetricName]),
            [];
        [{MetricName, [{type, Type}]}] ->
            get_metric_value(MetricName, Type)
    end.

%% Special handling for histogram.
%% Export a generic handler for converting metrics to a propertylist

-spec get_metric_value(folsom_metric_name_type(), folsom_metric_type()) -> proplist().

get_metric_value(MetricName, histogram) ->
    HistValues  = folsom_metrics:get_metric_value(MetricName),
    [{count, length(HistValues)},
     {mean_val, graphsom_util:mean(HistValues)}];
 
get_metric_value(MetricName, _Type) ->
    folsom_metrics:get_metric_value(MetricName).

