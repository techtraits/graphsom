-module(graphsom_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    start().

start() ->
    graphsom_sup:start_link().

stop(_State) ->
    ok.
