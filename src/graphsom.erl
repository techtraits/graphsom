-module(graphsom).

%% all graphsom api would go in here
-export([send_to_graphite/3, collect_metrics/3, collect_metrics/4]).
 
collect_metrics(MetricStr, Metrics, SystemMetrics, Prefix) ->
	MetricStr2 = collect_system_metrics(MetricStr, SystemMetrics, Prefix),
	collect_metrics(MetricStr2, Metrics, Prefix).

collect_system_metrics(SystemMetricStr, [SystemMetric | T], Prefix) ->
	SystemMetricStr2 = case catch SystemMetric of
		memory ->
			FullPrefix = prepend_prefix(Prefix, "memory."),
			format_multiple_metrics([], folsom_vm_metrics:get_memory(), gauge, FullPrefix);
		system_info ->
			SystemMetricStr;
		statistics ->
			SystemMetricStr;
		process_info ->
			SystemMetricStr;
		port_info ->
			SystemMetricStr;
		_ ->
			SystemMetricStr
	end,	
	collect_system_metrics(SystemMetricStr2, T, Prefix);

collect_system_metrics(SystemMetricStr, [], _Prefix) ->	
	SystemMetricStr.
	
%% Expects array of metric tuples {name, value}	
format_multiple_metrics(MetricString, [{MetricName, MetricValue} | T ], MetricType, Prefix) ->
	FullName = prepend_prefix(Prefix, MetricName),
	MetricString2 = string:concat(MetricString, format_metric(FullName, MetricType, MetricValue)), 
	format_multiple_metrics(MetricString2, T, MetricType, Prefix);

format_multiple_metrics(MetricString, [], _MetricType, _Prefix) ->
	MetricString.


%%TODO Collect History and Histogram
collect_metrics(MetricStr, [MetricName | T], Prefix) ->
    MetricStr2 = case catch folsom_metrics:get_metric_value(MetricName) of
                     {Error, Reason} ->
                         error_logger:info_msg("Error when getting metric from folsom_metrics: error: ~p, reason: ~p",[Error, Reason]),
                         MetricStr;
                     MetricValue ->                         
                         [{MetricName,[{type,MetricType}]}] = folsom_metrics:get_metric_info(MetricName),
                         FullMetricName = prepend_prefix(Prefix, MetricName),
                         string:concat(MetricStr , format_metric(FullMetricName, MetricType, MetricValue))
                 end,
    collect_metrics(MetricStr2, T, Prefix);

collect_metrics(MetricStr, [], _Prefix) ->	
	MetricStr.

format_metric(MetricName, MetricType, MetricValue) ->
    CurrentTime = current_time(),
    case MetricType of
    	gauge ->
    		io_lib:format("~s ~p ~w~n", [MetricName, MetricValue, CurrentTime]);
    	counter ->
    		io_lib:format("~s ~p ~w~n", [MetricName, MetricValue, CurrentTime]);
    	meter ->
    		[_, {one, One_Mean}, {five,Five_Mean}, {fifteen,Fifteen_Mean}, {mean, Mean}, _] = MetricValue,
    		StringAcc1 = string:concat([], io_lib:format("~s.last_one_mean ~p ~w~n", [MetricName,One_Mean, CurrentTime])),
    		StringAcc2 = string:concat(StringAcc1, io_lib:format("~s.last_five_mean ~p ~w~n", [MetricName,Five_Mean, CurrentTime])),
    		StringAcc3 = string:concat(StringAcc2, io_lib:format("~s.last_fifteen_mean ~p ~w~n", [MetricName,Fifteen_Mean, CurrentTime])),
    		string:concat(StringAcc3, io_lib:format("~s.all_time_mean ~p ~w~n", [MetricName, Mean, CurrentTime]))
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

prepend_prefix(Prefix, Name) ->
	PrefixStr = case is_atom(Prefix) of
    	true ->
        	atom_to_list(Prefix);
	    false ->
			Prefix
	end,
	
	NameStr = case is_atom(Name) of
    	true ->
        	atom_to_list(Name);
	    false ->
			Name
	end,
	string:concat(PrefixStr, NameStr).



current_time() ->    
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600. 