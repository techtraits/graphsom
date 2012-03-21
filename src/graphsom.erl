-module(graphsom).

%% all graphsom api would go in here
-export([register_folsom_metric/1, 
         registered_metrics/0, 
         start_reporting/0,
         stop_reporting/0]).

%% Includes

-include("graphsom.hrl").

%% API

-spec register_folsom_metric(folsom_metric_name_type()) -> ok | {error, term()}.

register_folsom_metric(FolsomMetric) ->
   graphsom_timer:register_folsom_metric(FolsomMetric).

-spec registered_metrics() -> ok | {error, term()}.

registered_metrics() ->
   graphsom_timer:get_registered_metrics().

-spec start_reporting() -> ok | {error, term()}.

start_reporting() ->
   graphsom_timer:start_reporting().

-spec stop_reporting() -> ok | {error, term()}.

stop_reporting() ->
   graphsom_timer:stop_reporting().
        
