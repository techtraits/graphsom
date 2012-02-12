-type start_type() :: normal
                    | {takeover, Node :: node()}
                    | {failover, Node :: node()}.


-type system_stats_type() :: list().

-type property() :: atom() | tuple().
-type config() :: [property()].
