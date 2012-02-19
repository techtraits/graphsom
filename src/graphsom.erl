-module(graphsom).

%% all graphsom api would go in here
-export([report_metrics/4]).

%% Includes

-include("graphsom.hrl").

%% API

-spec report_metrics(string(), pos_integer(), string(), vm_metrics_type()) -> ok | {error, term()}.

report_metrics(GraphiteHost, GraphitePort, GraphitePrefix, VmMetrics) ->
    Metrics = folsom_metrics:get_metrics(),
    io:format("List of user defined metrics: ~w ~n", [Metrics]),
    io:format("List of vm metrics ~w, ~n", [VmMetrics]),
    MetricStr = collect_metrics([], Metrics, VmMetrics, GraphitePrefix ),
    %% io:format("Metric string: ~p ~n", [lists:flatten(MetricStr)]),
    send_to_graphite(lists:flatten(MetricStr), GraphiteHost, GraphitePort).

-spec collect_metrics(string(), list(), vm_metrics_type(), string()) -> string().
 
collect_metrics(MetricStr, Metrics, VmMetrics, Prefix) ->
    MetricStr2 = collect_vm_metrics(MetricStr, VmMetrics, Prefix),
    io:format("MetricStr2 (VM Metrics): ~p, ~n", [MetricStr2]),
    collect_metrics(MetricStr2, Metrics, Prefix).

-spec collect_metrics(string(), list(), string()) -> string().

%% TODO Collect History and Histogram
collect_metrics(MetricStr, [MetricName | T], Prefix) ->
    MetricStr2 = case catch folsom_metrics:get_metric_value(MetricName) of
                     {Error, Reason} ->
                         error_logger:info_msg("Error when getting metric from folsom_metrics: error: ~p, reason: ~p",[Error, Reason]),
                         MetricStr;
                     MetricValue ->                         
                         [{MetricName,[{type,MetricType}]}] = folsom_metrics:get_metric_info(MetricName),
                         FullMetricName = prepend_prefix(Prefix, MetricName),
                         %% io:format("Full Metric Name: ~p, ~n", [FullMetricName]),
                         %% io:format("MetricStr: ~p ~n", [MetricStr]),
                         FormattedString = format_metric(FullMetricName, MetricType, MetricValue),
                         %% io:format("Formatted String: ~p, ~n", [FormattedString]),
                         string:concat(MetricStr , FormattedString)
                 end,
    collect_metrics(MetricStr2, T, Prefix);

collect_metrics(MetricStr, [], _Prefix) ->	
	MetricStr.
	
-spec collect_vm_metrics(string(), list(), string()) -> string().

collect_vm_metrics(VmMetricStr, [VmMetric | T], Prefix) ->
	VmMetricStr2 = case catch VmMetric of
		memory ->
			FullPrefix = prepend_prefix(Prefix, "memory."),
			format_multiple_metrics(VmMetricStr, folsom_vm_metrics:get_memory(), gauge, FullPrefix);
		system_info ->
			%%Too much data here figure out whats important
			%%FullPrefix = prepend_prefix(Prefix, "system_info."),
			%%error_logger:info_msg("~p ~n",[folsom_vm_metrics:get_system_info()]),
			VmMetricStr;
		statistics ->
			format_statistics(VmMetricStr, Prefix);
		process_info ->
			%%Too much data here figure out whats important
			%%FullPrefix = prepend_prefix(Prefix, "system_info."),
			%%error_logger:info_msg("~p ~n",[folsom_vm_metrics:get_process_info()]),
			VmMetricStr;
		port_info ->
			%%Too much data here figure out whats important
			%%FullPrefix = prepend_prefix(Prefix, "system_info."),
			%%error_logger:info_msg("~p ~n",[folsom_vm_metrics:get_port_info()]),
			VmMetricStr;
		_ ->
			VmMetricStr
	end,	
	collect_vm_metrics(VmMetricStr2, T, Prefix);

collect_vm_metrics(VmMetricStr, [], _Prefix) ->	
	VmMetricStr.

-spec format_statistics(string(), atom()) -> string(). 

format_statistics(VmMetricStr, Prefix) ->
%%	FullPrefix = prepend_prefix(Prefix, "stats."),
			[
				{context_switches, ContextSwithes},
				{garbage_collection, GarbageCollectionStats},
				{io, IOStats},
        		{reductions, ReductionStats},
    			{run_queue, RunQueue},
    			{runtime, RunTimeStats},
    			{wall_clock, WalClockStats}
			] = folsom_vm_metrics:get_statistics(),
			StatsString = string:concat(VmMetricStr, format_metric(prepend_prefix(Prefix,"context_switches."), gauge, ContextSwithes)),
			StatsString1 = format_multiple_metrics(
				StatsString, 
				GarbageCollectionStats, 
				gauge, 
				prepend_prefix(Prefix,"gc.")),
			StatsString2 = format_multiple_metrics(
				StatsString1, 
				IOStats, 
				gauge, 
				prepend_prefix(Prefix,"io.")),				
			StatsString3 = format_multiple_metrics(
				StatsString2, 
				ReductionStats, 
				gauge, 
				prepend_prefix(Prefix,"reductions.")),	
			StatsString4 = string:concat(StatsString3, format_metric(prepend_prefix(Prefix,"run_queue."), gauge, RunQueue)),
			StatsString5 = format_multiple_metrics(
				StatsString4, 
				RunTimeStats, 
				gauge, 
				prepend_prefix(Prefix,"runtime.")),	
			format_multiple_metrics(
				StatsString5, 
				WalClockStats, 
				gauge, 
				prepend_prefix(Prefix,"wall_clock.")).
	
-spec format_multiple_metrics(string(), list(), atom(), atom()) -> string().

%% Expects array of metric tuples {name, value}	
format_multiple_metrics(MetricString, [{MetricName, MetricValue} | T ], MetricType, Prefix) ->
	FullName = prepend_prefix(Prefix, MetricName),
	MetricString2 = string:concat(MetricString, format_metric(FullName, MetricType, MetricValue)), 
	format_multiple_metrics(MetricString2, T, MetricType, Prefix);

format_multiple_metrics(MetricString, [], _MetricType, _Prefix) ->
	MetricString.

-spec format_metric(atom(), atom(), number()) -> string().

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

-spec send_to_graphite(string(), string(), pos_integer()) -> ok | {error, term()}.
   
send_to_graphite(MetricStr, GraphiteHost, GraphitePort) ->
    case gen_tcp:connect(GraphiteHost, GraphitePort, [list, {packet, 0}]) of
        {ok, Sock} ->
            gen_tcp:send(Sock, MetricStr),
            gen_tcp:close(Sock),
            io:format("Metrics updated to graphite at ~p ~n", [GraphiteHost]),
            ok;
        {error, Reason} ->
            io:format("Failed to connect to graphite host ~p for reason ~p ~n", [GraphiteHost, Reason]),
            {error, Reason}
    end. 

%% private api

-spec prepend_prefix(term(), term()) -> string().

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

-spec current_time() -> pos_integer().

current_time() ->    
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600. 
