-module(graphsom).

%% all graphsom api would go in here
-export([report_metrics/5]).

%% Includes

-include("graphsom.hrl").

%% API

-spec report_metrics(string(), pos_integer(), string(), list(), boolean()) -> ok | {error, term()}.

report_metrics(GraphiteHost, GraphitePort, GraphitePrefix, VmMetrics, ReportAllMetrics) ->
    %% check whether to report all user created metrics or not
    Metrics = case ReportAllMetrics of
                  true ->
                      folsom_metrics:get_metrics();
                  _ ->
                      []
              end,
    io:format("List of metrics: ~w ~n", [Metrics]),
    io:format("List of vm metrics ~w, ~n", [VmMetrics]),
    CurrentTime = graphsom_util:current_time(),
    VmMetricStr= graphsom_folsom:stringify_vm_metrics(VmMetrics, GraphitePrefix, CurrentTime),
    UserMetricStr = graphsom_folsom:stringify_metrics(Metrics, GraphitePrefix, CurrentTime),
    MetricStr = lists:flatten([VmMetricStr, UserMetricStr]),
    io:format("Metric string: ~s ~n", [MetricStr]),
    graphsom_graphite:report(MetricStr, GraphiteHost, GraphitePort).
