-record(window, {id,
                 pid,
                 has_border = false,
                 %pct,
                 x = 0,
                 y = 0,
                 h = 0,
                 w = 0}).

-type window() :: {integer(), pid()}.
-type pos() :: first | rest.
-type type() :: row | col.

-record(up, {type = row :: type(),
             pos = first :: pos(),
             primary = undefined :: window() | undefined,
             parent = undefined :: window() | undefined,
             new = undefined :: window() | undefined}).
