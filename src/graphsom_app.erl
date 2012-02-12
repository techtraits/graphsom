-module(graphsom_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% Includes
-include("graphsom.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================


-spec start(start_type(), term()) -> {ok, pid()} | {error, term()}.

start(_StartType, _StartArgs) ->
    start().

-spec start() -> {ok, pid()} | {error, term()}.

start() ->
    Config = application:get_all_env(graphsom),
    graphsom_sup:start_link(Config).

-spec stop([]) -> ok. 

stop([]) ->
    ok.
    
