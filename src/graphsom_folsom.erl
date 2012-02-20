-module(graphsom_folsom).

-include("graphsom.hrl").
 
-export([stringify_metrics/4]).


-spec stringify_metrics(list(), list(),  string(), pos_integer()) -> string().

stringify_metrics(Metrics, VmMetrics, Prefix, CurTime) ->
    lists:flatten([stringify_vm_metric(VmMetric, Prefix, CurTime) || VmMetric <- VmMetrics],
                  [stringify_metric(Metric, Prefix, CurTime) || Metric <- Metrics]).

-spec stringify_vm_metric(folsom_vm_metric_type(), string(), pos_integer()) -> string().

stringify_vm_metric(VmMetric, Prefix, CurTime) ->
    MetricValue = get_vm_metric_value(VmMetric),
    graphsom_graphite:stringify_proplist_metric(VmMetric, MetricValue, Prefix, CurTime, "").

-spec stringify_metric(string(), string(), pos_integer()) -> string().

stringify_metric(MetricName, Prefix, CurTime) ->
    MetricValue = get_metric_value(MetricName),
    graphsom_graphite:stringify_proplist_metric(MetricName, MetricValue, Prefix, CurTime, "").

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

