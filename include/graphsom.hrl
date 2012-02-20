-type start_type() :: normal
                    | {takeover, Node :: node()}
                    | {failover, Node :: node()}.

-type folsom_vm_metric_type() :: memory
                               | system_info
                               | process_info
                               | statistics
                               | port_info.

-type property() :: atom() | tuple().
-type config() :: [property()].
-type proplist() :: config().

-type folsom_metric_name_type() :: binary() 
                          | atom()
                          | string().

-type folsom_metric_type() :: counter 
                            | gauge
                            | meter
                            | histogram
                            | history.

-type folsom_metric() :: {folsom_metric_name_type(), [{type, folsom_metric_type()}]}.

-type folsom_metric_value_type() :: number() | list().
