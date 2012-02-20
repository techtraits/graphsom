-module(graphsom).

%% all graphsom api would go in here
-export([report_metrics/5]).

%% Includes

-include("graphsom.hrl").

%% API

-spec report_metrics(string(), pos_integer(), string(), list(), boolean()) -> ok | {error, term()}.

report_metrics(GHost, GPort, GPrefix, VmMetrics, true) ->
    report_metrics(folsom_metrics:get_metrics(), VmMetrics, GHost, GPort, GPrefix, graphsom_util:current_time());

report_metrics(GHost, GPort, GPrefix, VmMetrics, false) ->    
    report_metrics([], VmMetrics, GHost, GPort, GPrefix, graphsom_util:current_time()).

%% Internal API

-spec report_metrics(list(), list(), string(), pos_integer(), string(), pos_integer()) -> ok | {error | term()}.

report_metrics(Metrics, VmMetrics, GHost, GPort, GPrefix, CurTime) ->
    MetricStr = stringify_metrics(Metrics, VmMetrics, GPrefix, CurTime),
    io:format("Metric string: ~s ~n", [MetricStr]),
    graphsom_graphite:report(MetricStr, GHost, GPort).

-spec stringify_metrics(list(), list(),  string(), pos_integer()) -> string().

stringify_metrics(Metrics, VmMetrics, GPrefix, CurTime) ->
    MList = graphsom_folsom:get_metrics(Metrics, VmMetrics),
    lists:flatten([graphsom_graphite:stringify_proplist_metric(Name, Val, GPrefix, CurTime, "")|| {Name, Val} <- MList]).
