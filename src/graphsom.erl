-module(graphsom).

%% all graphsom api would go in here
-export([send_to_graphite/3, collect_metrics/2]).
 
%% TODO collect non-gauge metrics
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
    		io_lib:format("~p ~p ~w ~n", [MetricName,MetricValue, CurrentTime])
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