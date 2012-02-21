-module(graphsom).

%% all graphsom api would go in here
-export([register_folsom_metric/1]).

%% Includes

-include("graphsom.hrl").

%% API

-spec register_folsom_metric(folsom_metric_name_type()) -> ok | {error, term()}.

register_folsom_metric(FolsomMetric) ->
    case whereis(graphsom_timer) of
        undefined ->
            {error, graphsom_timer_not_found};
        Gt ->
            gen_server:cast(Gt, {register, FolsomMetric}),
            ok
    end.
