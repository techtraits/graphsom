-type start_type() :: normal
                    | {takeover, Node :: node()}
                    | {failover, Node :: node()}.


-type vm_metrics_type() :: list().

-type property() :: atom() | tuple().
-type config() :: [property()].
