-module(graphsom_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Include

-include("graphsom.hrl").

%% Helper macro for declaring children of supervisorp
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================
-spec start_link() -> {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================

%% Supervisor callbacks
%% ===================================================================


-spec init([]) -> ignore | {ok,{{one_for_all, non_neg_integer(),non_neg_integer()} | {one_for_one ,non_neg_integer(),non_neg_integer()} | {rest_for_one ,non_neg_integer(),non_neg_integer()} | {simple_one_for_one ,non_neg_integer(),non_neg_integer()},[{_,{atom() | tuple(),atom(), undefined | [any()]}, permanent | temporary | transient, brutal_kill | infinity | non_neg_integer(), supervisor | worker, dynamic | [atom() | tuple()]}]}}.

init([]) ->
    graphsom_ets:init(),
    graphsom_config:init(),
    graphsom_metrics:init(),
    %% adding folsom_sup and graphsom to graphsom_sup's supervision tree
    Folsom = {folsom,
              {folsom_sup, start_link, []},
              permanent,
              5000,
              supervisor,
              [folsom_sup]
             },
    GraphsomTimer = {graphsom_timer,
                      {graphsom_timer, start_link, []},
                      permanent, 
                      5000, 
                      worker, 
                      [graphsom_timer]},
    {ok, { {one_for_one, 100, 10}, [Folsom, GraphsomTimer]} }.

