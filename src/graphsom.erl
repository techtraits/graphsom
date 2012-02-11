-module(graphsom).

%% all graphsom api would go in here
-export([send_to_graphite/3, collect_metrics/2, collect_metrics/3]).
 
collect_metrics(MetricStr, Metrics, SystemMetrics) ->
	collect_metrics(MetricStr, Metrics).

%%TODO Collect History and Histogram
collect_metrics(MetricStr, [MetricName | T]) ->
    MetricStr2 = case catch folsom_metrics:get_metric_value(MetricName) of
                     {Error, Reason} ->
                         error_logger:info_msg("Error when getting metric from folsom_metrics: error: ~p, reason: ~p",[Error, Reason]),
                         MetricStr;
                     MetricValue ->                         
                         [{MetricName,[{type,MetricType}]}] = folsom_metrics:get_metric_info(MetricName),
                         error_logger:info_msg("Got value ~p for metric ~p of type ~p \n",[MetricValue, MetricName, MetricType]),                     
                         string:concat(MetricStr , format_metric(MetricName, MetricType, MetricValue))

                 end,
    collect_metrics(MetricStr2, T);

collect_metrics(MetricStr, []) ->	
	MetricStr.

format_metric(MetricName, MetricType, MetricValue) ->
    CurrentTime = current_time(),
    case MetricType of
    	gauge ->
    		io_lib:format("~p ~p ~w~n", [MetricName,MetricValue, CurrentTime]);
    	counter ->
    		io_lib:format("~p ~p ~w~n", [MetricName,MetricValue, CurrentTime]);
    	meter ->
    		[_, {one, One_Mean}, {five,Five_Mean}, {fifteen,Fifteen_Mean}, {mean, Mean}, _] = MetricValue,
    		StringAcc1 = string:concat([], io_lib:format("~p.last_one_mean ~p ~w~n", [MetricName,One_Mean, CurrentTime])),
    		StringAcc2 = string:concat(StringAcc1, io_lib:format("~p.last_five_mean ~p ~w~n", [MetricName,Five_Mean, CurrentTime])),
    		StringAcc3 = string:concat(StringAcc2, io_lib:format("~p.last_fifteen_mean ~p ~w~n", [MetricName,Fifteen_Mean, CurrentTime])),
    		string:concat(StringAcc3, io_lib:format("~p.all_time_mean ~p ~w~n", [MetricName,Mean, CurrentTime]))
    	end.

   
send_to_graphite(MetricStr, GraphiteHost, GraphitePort) ->
    case gen_tcp:connect(GraphiteHost, GraphitePort, [list, {packet, 0}]) of
        {ok, Sock} ->
            gen_tcp:send(Sock, MetricStr),
            gen_tcp:close(Sock),
            ok;
        {error, Reason} ->
            error_logger:error_msg("Failed to connect to graphite: ~p", [Reason]),
            {error, Reason}
    end. 

current_time() ->    
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600. 