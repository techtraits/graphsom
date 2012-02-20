-module(graphsom_folsom).

-include("graphsom.hrl").
 
-export([stringify_vm_metrics/3, stringify_metrics/3]).


-spec stringify_vm_metrics(list(), string(), pos_integer()) -> string().

stringify_vm_metrics(VmMetrics, Prefix, CurTime) ->
    lists:flatten([stringify_vm_metric(VmMetric, Prefix, CurTime) || VmMetric <- VmMetrics]).

-spec stringify_vm_metric(folsom_vm_metric_type(), string(), pos_integer()) -> string().

stringify_vm_metric(memory, Prefix, CurTime) ->
    MetricValue = folsom_vm_metrics:get_memory(),
    graphsom_graphite:stringify_proplist_metric(memory, MetricValue, Prefix, CurTime, "");

stringify_vm_metric(system_info, Prefix, CurTime) ->
    MetricValue = folsom_vm_metrics:get_system_info(),
    graphsom_graphite:stringify_proplist_metric(system_info, MetricValue, Prefix, CurTime, "");

stringify_vm_metric(process_info, Prefix, CurTime) ->
    MetricValue = folsom_vm_metrics:get_process_info(),
    graphsom_graphite:stringify_proplist_metric(process_info, MetricValue, Prefix, CurTime, "");

stringify_vm_metric(statistics, Prefix, CurTime) ->
    MetricValue = folsom_vm_metrics:get_statistics(),
    graphsom_graphite:stringify_proplist_metric(statistics, MetricValue, Prefix, CurTime, "");

stringify_vm_metric(port_info, Prefix, CurTime) ->
    MetricValue = folsom_vm_metrics:get_memory(),
    graphsom_graphite:stringify_proplist_metric(port_info, MetricValue, Prefix, CurTime, "").

-spec stringify_metrics(list(), string(), pos_integer()) -> string().

stringify_metrics(Metrics, Prefix, CurTime) ->
    lists:flatten([stringify_metric(Metric, Prefix, CurTime) || Metric <- Metrics]).

-spec stringify_metric(string(), string(), pos_integer()) -> string().

stringify_metric(MetricName, Prefix, CurTime) ->
    MetricValue = get_metric_value(MetricName),
    graphsom_graphite:stringify_proplist_metric(MetricName, MetricValue, Prefix, CurTime, "").

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

