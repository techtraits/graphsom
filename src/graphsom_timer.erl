-module(graphsom_timer).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]). 
-export([handle_info/2, terminate/2, code_change/3]).
 
-export([start_link/5, stop/0]).

-include("graphsom.hrl").

-record(state, {report_interval,  % ms interval between stat reporting
                report_timer, 	  % TRef of interval timer
                graphite_host,    % graphite server host
                graphite_port,	  % graphite server port
                graphite_prefix,  % prefix added to all graphite keys
                vm_metrics	  % list of vm metrics to report
               }).               
               
-type state() :: #state{}.

-spec start_link(pos_integer(), string(), pos_integer(), string(), vm_metrics_type()) -> {ok, pid()} | {error, term()}.

start_link(ReportIntervalMs, GraphiteHost, GraphitePort, Prefix, VmMetrics) ->
    io:format("graphsom_timer started ~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE,  
                          [ReportIntervalMs, GraphiteHost, GraphitePort, Prefix, VmMetrics], []).

-spec init(list()) -> {ok, state()}.

init([ReportIntervalMs, GraphiteHost, GraphitePort, Prefix, VmMetrics]) ->
    io:format("graphsom will report stats to ~p:~p every ~p ms ~n",
              [ GraphiteHost, GraphitePort, ReportIntervalMs ]),
    {ok, Tref} = timer:apply_interval(ReportIntervalMs, gen_server, cast,  [?MODULE, report]),                     
    State = #state{ 
      report_interval = ReportIntervalMs,
      report_timer = Tref,
      graphite_host = GraphiteHost,
      graphite_port = GraphitePort,
      graphite_prefix = Prefix,
      vm_metrics = VmMetrics
     },
    io:format("graphsom_timer Vm Metrics: ~w ~n", [VmMetrics]),
    {ok, State}.

-type cast_msg_type() :: report | stop | term().

-spec handle_cast(cast_msg_type(), state()) -> {noreply, state()} | {stop, normal, state()}.

handle_cast(report, State) -> 
    graphsom:report_metrics(State#state.graphite_host, State#state.graphite_port, 
                            State#state.graphite_prefix, State#state.vm_metrics),
    {noreply, State};
	
handle_cast(stop, State) ->
   {stop, normal, State};
   
handle_cast(_, State) -> 
	{noreply, State}.

-spec handle_call(term(), term(), state()) -> {reply, ok, state()}.

handle_call(_,_,State) ->
	{reply,ok,State}.

-spec terminate(term(), state()) -> ok.

terminate(_Reason, _State) ->
	ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
	
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}. 

-spec handle_info(term(), state()) -> {noreply, state()}.
	
handle_info(_Info, State) ->
	{noreply,State}.

-spec stop() -> ok.
	
stop() ->
    gen_server:cast(?MODULE, stop).	

   
