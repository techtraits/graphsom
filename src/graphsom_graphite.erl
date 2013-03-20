-module(graphsom_graphite).

-include("graphsom.hrl").

-export([report/3, stringify_proplist_metric/5]).

-spec report(string(), string(), pos_integer()) -> ok | {error, term()}.
   
report("", _GraphiteHost, _GraphitePort) ->
    ok;
report(MetricStr, GraphiteHost, GraphitePort) ->
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

%% The core function that converts various types of metrics to string

-spec stringify_proplist_metric(folsom_metric_name_type(), folsom_metric_value_type(), string(), pos_integer(), string()) -> string().

stringify_proplist_metric(MetricName, MetricValue, Prefix, CurTime, Str) when is_number(MetricValue) ->
    io_lib:format("~s~s.~s ~w ~w~n", [Str, Prefix, MetricName, MetricValue, CurTime]);

stringify_proplist_metric(MetricName, MetricValue, Prefix, CurTime, Str) when is_integer(MetricName) ->
    stringify_proplist_metric(integer_to_list(MetricName), MetricValue, Prefix, CurTime, Str);

stringify_proplist_metric(MetricName, {SubName, MetricValue}, Prefix, CurTime, Str) ->
    NewPrefix = io_lib:format("~s.~s", [Prefix, MetricName]),
    stringify_proplist_metric(SubName, MetricValue, NewPrefix, CurTime, Str);

stringify_proplist_metric(MetricName, [MetricValue | T], Prefix, CurTime, Str) ->
    Str1 = stringify_proplist_metric(MetricName, MetricValue, Prefix, CurTime, Str),
    stringify_proplist_metric(MetricName, T, Prefix, CurTime, Str1);

stringify_proplist_metric(_MetricName, _MetricValue, _Prefix, _CurTime, Str) ->
    Str.
