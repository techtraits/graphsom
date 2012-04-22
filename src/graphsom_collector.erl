-module(graphsom_collector).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]). 
-export([handle_info/2, terminate/2, code_change/3]).

-export([
         collect_now/0,
         start_collecting/0,
         stop_collecting/0
]).
 
-export([start_link/0, stop/0]).

-include("graphsom.hrl").

 % whether to collect metrics or not
-record(state, { collect = false :: boolean() }).               
               
-type state() :: #state{}.

-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    io:format("graphsom_collector started ~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(list()) -> {ok, state()}.

init([]) ->
    {ok, #state{collect = false}}.

-spec collect() -> ok.

collect() ->
    %% collect metrics if collecting
    _ = gen_server:cast(?MODULE, collect),
    %% update timer interval
    Interval = graphsom_config:min_interval(),
    %% collect again after Interval
    {ok,_Tref} = timer:apply_after(Interval, ?MODULE, collect,  []),
    ok.

-spec collect_now() -> ok | {error, term()}.

collect_now() ->
    gen_server:cast(?MODULE, collect_now).

-spec start_collecting() -> ok | {error, term()}.

start_collecting() -> 
    gen_server:cast(?MODULE, start_collecting).

-spec stop_collecting() -> ok | {error, term()}.

stop_collecting() ->
    gen_server:cast(?MODULE, stop_collecting).

-type cast_msg_type() :: collect | stop | start_collecting | stop_collecting | term().

-spec handle_cast(cast_msg_type(), state()) -> {noreply, state()} | {stop, normal, state()}.

%% don't do anything if collect is set to false
handle_cast(collect, State = #state{ collect = false }) ->
    {noreply, State};

handle_cast(collect, State = #state{ collect = true }) ->
    handle_cast(collect_now, State);

handle_cast(collect_now, _State) ->
    MetricValues = graphsom_metrics:all(),
    TimeNow = graphsom_util:current_time(),
    %% store a snapshot metrics at this time
    MetricDump = {graphsom_metrics_dump, TimeNow, MetricValues},
    true = ets:insert(?GRAPHSOM_METRICS_STORE, MetricDump),
    {noreply, _State};

handle_cast(start_collecting, State = #state{}) ->
    {noreply, State#state{ collect = true}};

handle_cast(stop_collecting, State = #state{}) ->
    {noreply, State#state{ collect = false}};

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
