%% Types defined
-type task() :: #{name => bitstring(),
                 command => bitstring(),
                 requires => [bitstring()]}.

-type job() :: [task()].