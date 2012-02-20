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
    CurrentTime = current_time(),
    VmMetricStr= stringify_folsom_vm_metrics(VmMetrics, GraphitePrefix, CurrentTime),
    UserMetricStr = stringify_folsom_metrics(Metrics, GraphitePrefix, CurrentTime),
    MetricStr = lists:flatten([VmMetricStr, UserMetricStr]),
    io:format("Metric string: ~s ~n", [MetricStr]),
    send_to_graphite(MetricStr, GraphiteHost, GraphitePort).

%% private api

-spec stringify_folsom_vm_metrics(list(), string(), pos_integer()) -> string().

stringify_folsom_vm_metrics(VmMetrics, Prefix, CurTime) ->
    lists:flatten([stringify_folsom_vm_metric(VmMetric, Prefix, CurTime) || VmMetric <- VmMetrics]).

-spec stringify_folsom_vm_metric(folsom_vm_metric_type(), string(), pos_integer()) -> string().

stringify_folsom_vm_metric(memory, Prefix, CurTime) ->
    MetricValue = folsom_vm_metrics:get_memory(),
    stringify_proplist_metric(memory, MetricValue, Prefix, CurTime, "");

stringify_folsom_vm_metric(system_info, Prefix, CurTime) ->
    MetricValue = folsom_vm_metrics:get_system_info(),
    stringify_proplist_metric(system_info, MetricValue, Prefix, CurTime, "");

stringify_folsom_vm_metric(process_info, Prefix, CurTime) ->
    MetricValue = folsom_vm_metrics:get_process_info(),
    stringify_proplist_metric(process_info, MetricValue, Prefix, CurTime, "");

stringify_folsom_vm_metric(statistics, Prefix, CurTime) ->
    MetricValue = folsom_vm_metrics:get_statistics(),
    stringify_proplist_metric(statistics, MetricValue, Prefix, CurTime, "");

stringify_folsom_vm_metric(port_info, Prefix, CurTime) ->
    MetricValue = folsom_vm_metrics:get_memory(),
    stringify_proplist_metric(port_info, MetricValue, Prefix, CurTime, "").

-spec stringify_folsom_metrics(list(), string(), pos_integer()) -> string().

stringify_folsom_metrics(Metrics, Prefix, CurTime) ->
    lists:flatten([stringify_folsom_metric(Metric, Prefix, CurTime) || Metric <- Metrics]).

-spec stringify_folsom_metric(string(), string(), pos_integer()) -> string().

stringify_folsom_metric(MetricName, Prefix, CurTime) ->
    MetricValue = get_folsom_metric_value(MetricName),
    stringify_proplist_metric(MetricName, MetricValue, Prefix, CurTime, "").

-spec get_folsom_metric_value(folsom_metric_name_type()) -> folsom_metric_value_type().

get_folsom_metric_value(MetricName) ->
    case catch folsom_metrics:get_metric_info(MetricName) of
        {'EXIT', _ } ->
            io:format("Metric ~s does not exist ~n", [MetricName]),
            [];
        [{MetricName, [{type, Type}]}] ->
            get_folsom_metric_value(MetricName, Type)
    end.

%% Special handling for histogram.
%% Export a generic handler for converting metrics to a propertylist

-spec get_folsom_metric_value(folsom_metric_name_type(), folsom_metric_type()) -> proplist().

get_folsom_metric_value(MetricName, histogram) ->
    HistValues  = folsom_metrics:get_metric_value(MetricName),
    [{count, length(HistValues)},
     {mean_val, mean(HistValues)}];

get_folsom_metric_value(MetricName, _Type) ->
    folsom_metrics:get_metric_value(MetricName).

%% The core function that converts various types of metrics to string

-spec stringify_proplist_metric(folsom_metric_name_type(), folsom_metric_value_type(), string(), pos_integer(), string()) -> string().

stringify_proplist_metric(MetricName, MetricValue, Prefix, CurTime, Str) when is_number(MetricValue) ->
    io_lib:format("~s~s.~s ~B ~w~n", [Str, Prefix, MetricName, MetricValue, CurTime]);

stringify_proplist_metric(MetricName, {SubName, MetricValue}, Prefix, CurTime, Str) ->
    NewPrefix = io_lib:format("~s.~s", [Prefix, MetricName]),
    stringify_proplist_metric(SubName, MetricValue, NewPrefix, CurTime, Str);

stringify_proplist_metric(MetricName, [MetricValue | T], Prefix, CurTime, Str) ->
    Str1 = stringify_proplist_metric(MetricName, MetricValue, Prefix, CurTime, Str),
    stringify_proplist_metric(MetricName, T, Prefix, CurTime, Str1);

stringify_proplist_metric(_MetricName, _MetricValue, _Prefix, _CurTime, Str) ->
    Str.

-spec send_to_graphite(string(), string(), pos_integer()) -> ok | {error, term()}.
   
send_to_graphite(MetricStr, GraphiteHost, GraphitePort) ->
    case gen_tcp:connect(GraphiteHost, GraphitePort, [list, {packet, 0}]) of
        {ok, Sock} ->
            _ = gen_tcp:send(Sock, MetricStr),
            gen_tcp:close(Sock),
            io:format("Metrics updated to graphite at ~p ~n", [GraphiteHost]),
            ok;
        {error, Reason} ->
            io:format("Failed to connect to graphite host ~p for reason ~p ~n", [GraphiteHost, Reason]),
            {error, Reason}
    end. 

-spec mean(list()) -> number().

mean(List) -> mean(List, 0, 0).

-spec mean(list(), non_neg_integer(), number()) -> number().

mean([H|T], C, S) -> mean(T, C + 1, S + H);
mean([], 0, _S) -> 0; 
mean([], C, S) -> S/C.

-spec current_time() -> pos_integer().

current_time() ->    
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600. 
