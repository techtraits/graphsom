graphsom
===============

graphsom dumps provides visualizations for metrics collected using [folsom](https://github.com/boundary/folsom). Currently we support only [Graphite](http://graphite.wikidot.com/) visualizations but we will be adding support for multiple backends soon. 

Note: [Graphite](http://graphite.wikidot.com/) is good, despite the
website being really ghetto. 

Status
------

* Supports all [folsom](https://github.com/boundary/folsom) metric
  types (`gauge`, `counter`, `meter`, `histogram` and `history`)
* Supports all [folsom](https://github.com/boundary/folsom) VM metrics
  including `memory`,`statistics`, `process_info`, `system_info` and `port_info`
* Support for other visualization backends is coming very soon

Getting Started
--------------

Prerequisites 

* [Download](https://launchpad.net/graphite/+download),
  [Install](http://graphite.wikidot.com/installation) and run graphite
* [Download](https://github.com/downloads/basho/rebar/rebar) or [build](https://github.com/basho/rebar) rebar
* Clone [graphsom](https://github.com/techtraits/graphsom.git)

Building graphsom

    cd graphsom
	chmod u+x rebar  # Make [rebar](https://github.com/basho/rebar) executable  
    
Get all dependencies (only [folsom](https://github.com/boundary/folsom))

    mkdir deps
    ./rebar get-deps
    
Compile

    ./rebar compile
    
Creating a release

    mkdir rel
    cd rel
    ../rebar create-node nodeid=graphsom
    
At this point the `rel` folder should have a `reltool.config` (release
tool configuration). We need to tell the `reltool.config` where to
find `graphsom` and `folsom` applications. This can be done by
specifying `lib_dir` as follows:

    {lib_dirs, ["../../", "../deps/"]},
    
We just need to make one last change before we can generate a release
and run it. We want folsom application to be included into the
release. This can be done as follows:

    {app, stdlib, [{incl_cond, include}]},
    {app, kernel, [{incl_cond, include}]},
    {app, folsom, [{incl_cond, include}]},
    **{app, folsom, [{incl_cond, include}]},**
    {app, graphsom, [{incl_cond, include}]}
    
Generating a release (a self-containing deploy-able directory
containing all the applications for your node):

    ./rebar generate

Running graphsom node:

    ./rel/graphsom/bin/graphsom console

Or alternatively Makefile wrappers can be used for convenience. 

Graphsom will dump all `folsom metrics` every `30 seconds`
to the local graphite backend. The configuration for graphsom can be
added to `rel/files/sys.config`. An example configuration:
    
    {graphsom, [
             {report_interval, 30000},     %% report interval (ms)
             {graphite_host, "techtraits.com"}, %% defaults to localhost
             {graphite_port, 2003},        %% defaults to 2003
             {graphite_prefix, "graphsom"}, %% prefix added to all metrics
             {vm_metrics, [memory, statistics]},   %% defaults to []
             {report_all_folsom_metrics, false}   %% defaults to false
            ]}
           
* `graphite_prefix` can be used to specify the prefix prepended to all
metric names. For example, "response_time" would be reported as
"graphsom.response_time". 

* `vm_metrics` can be used to report folsom VM metrics

* `report_all_folsom_metrics` if set to true reports all folsom
  metrics at the end of a reporting interval. Alternatively, if it is
  `false`, graphsom offers a `register_folsom_metric` API for
  registering only those metrics that are to be reported to graphite.

**Note:** Graphsom takes care of starting `folsom`.

In the console you can:

Add Some metrics:

    folsom_metrics:new_counter(metric_name_1).
    folsom_metrics:new_meter(metric_name_2).

Update values for the metrics:

	folsom_metrics:notify({metric_name_1, {inc, 100}}).
    folsom_metrics:notify({metric_name_2, 300}).

Register these metrics with `graphsom`:

    graphsom:register_folsom_metric(metric_name_1).
    graphsom:register_folsom_metric(metric_name_2).
    
Tell graphsom to start reporting:

    graphsom:start_reporting().

Check graphite for the values   

Authors 
------

Bilal Sheikh (<bilal@techtraits.com>)  
Usman Ismail (<usman@techtraits.com>)
