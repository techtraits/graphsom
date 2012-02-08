-module(graphsom_server).
-behaviour(gen_server).

 -export([init/1, handle_call/3, handle_cast/2]). 
 -export([handle_info/2, terminate/2, code_change/3]).
 
 -export([stop/0]).

 -export([start/1]).
  
 -record(state, {report_interval, 	% ms interval between stat reporting
                 report_timer, 		% TRef of interval timer
                 graphite_host, 	% graphite server host
                 graphite_port 		% graphite server port
               }).               
               
start([ReportIntervalMs, GraphiteHost, GraphitePort]) ->  
	gen_server:start_link({local, ?MODULE}, ?MODULE,  
                              [ReportIntervalMs, GraphiteHost, GraphitePort], []).

init([ReportIntervalMs, GraphiteHost, GraphitePort]) ->
    error_logger:info_msg("graphsom will report stats to ~p:~w every ~wms\n",
                          [ GraphiteHost, GraphitePort, ReportIntervalMs ]),
    {ok, Tref} = timer:apply_interval(ReportIntervalMs, gen_server, cast,  [?MODULE, {report}]),                          
    State = #state{ 
      report_interval = ReportIntervalMs,
      report_timer = Tref,
      graphite_host = GraphiteHost,
      graphite_port = GraphitePort
     },
    {ok, State}.

handle_cast({report}, State) -> 
	%% Msg = collect_metrics([], State, gb_trees:to_list(State#state.metrics)),
	%% send_to_graphite(lists:flatten(Msg),State),
	{noreply, State};
	
	
handle_cast(stop, State) ->
   {stop, normal, State};
   
handle_cast(_, State) -> 
	{noreply, State}.

handle_call(_,_,State) ->
	{reply,ok,State}.

terminate(_Reason, _State) ->
	ok.
	
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}. 
	
handle_info(_Info, State) ->
	{noreply,State}.
	
stop() ->
    gen_server:cast(?MODULE, stop).	

%% collect_metrics(MsgAcc, State, [{Name, Metric}|Tail]) ->
%%	UMsgAcc = case Metric#metric.type of 
%%		guage ->
%%			Value = gen_server:call(Metric#metric.callback_module,{get_metric, Name, Metric#metric.args}),
%%			error_logger:info_msg("Got value ~p for metric ~p \n",[Value, Metric#metric.name]),
%%			string:concat(MsgAcc , io_lib:format("~p ~p ~w ~n", [Name , Value, current_time()]));
%%		_ ->
%%			error_logger:info_msg("Unknown Metric type ~p for metric ~p\n",[Metric#metric.type, Metric#metric.name]),
%%			MsgAcc
%%	end,
%%	collect_metrics(UMsgAcc, State, Tail);
%%
%% collect_metrics(MsgAcc, _State, []) ->	
%%	MsgAcc.
   
%% send_to_graphite(Msg, State) ->
%%    case gen_tcp:connect(State#state.graphite_host,
%%                         State#state.graphite_port,[list, {packet, 0}]) of
%%        {ok, Sock} ->
%%            gen_tcp:send(Sock, Msg),
%%            gen_tcp:close(Sock),
%%            ok;
%%        {error, Reason} ->
%%            error_logger:error_msg("Failed to connect to graphite: ~p", [Reason]),
%%            {error, Reason}
%%    end. 

%% current_time() ->    
%%	calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600.    
