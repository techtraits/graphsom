-module(graphsom).

%% all graphsom api would go in here
-export([report_metrics/5]).

%% Includes

-include("graphsom.hrl").

%% API

-spec report_metrics(string(), pos_integer(), string(), list(), boolean()) -> ok | {error, term()}.

report_metrics(GraphiteHost, GraphitePort, GraphitePrefix, VmMetrics, true) ->
    report_metrics(folsom_metrics:get_metrics(), VmMetrics, GraphiteHost, GraphitePort, GraphitePrefix, graphsom_util:current_time());

report_metrics(GraphiteHost, GraphitePort, GraphitePrefix, VmMetrics, false) ->    
    report_metrics([], VmMetrics, GraphiteHost, GraphitePort, GraphitePrefix, graphsom_util:current_time()).

%% Internal API

-spec report_metrics(list(), list(), string(), pos_integer(), string(), pos_integer()) -> ok | {error | term()}.

report_metrics(Metrics, VmMetrics, GraphiteHost, GraphitePort, GraphitePrefix, CurTime) ->
    VmMetricStr= graphsom_folsom:stringify_vm_metrics(VmMetrics, GraphitePrefix, CurTime),
    UserMetricStr = graphsom_folsom:stringify_metrics(Metrics, GraphitePrefix, CurTime),
    MetricStr = lists:flatten([VmMetricStr, UserMetricStr]),
    io:format("Metric string: ~s ~n", [MetricStr]),
    graphsom_graphite:report(MetricStr, GraphiteHost, GraphitePort).

