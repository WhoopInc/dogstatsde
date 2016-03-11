# A dogstatsd client for Erlang #

DogStatsD is Datadog's extension of StatsD. It adds tags to the metrics.

## Configure ##

The defaults assume that you're running a statsd server on localhost (true if the agent is installed locally).

There are a number of configuration settings. You can either provide them as environment variables in ALL_CAPS
or in your `sys.config` file in all_lowercase, under the `dogstatsd` key.

    | name               | type    | default       | info                                                                                  |
    | ----               | ----    | -------       | ----                                                                                  |
    | AGENT_ADDRESS      | string  | `"localhost"` | Hostname or IP where we can send the StatsD UDP packets                               |
    | AGENT_PORT         | integer | `8125`        | Port that the StatsD agent is listening on                                            |
    | GLOBAL_PREFIX      | string  | `""`          | Prefix to attach before all metric names. The `.` will be inserted for you            |
    | GLOBAL_TAGS        | map     | `#{}`         | Tags to attach to all metrics                                                         |
    | SEND_METRICS       | boolean | `true`        | Set to `false` when you're running tests to disable sending any metrics               |
    | VM_STATS           | boolean | `true`        | Collect stats on the Erlang VM?                                                       |
    | VM_STATS_DELAY     | integer | `60000`       | Time in ms between collection Erlang VM stats                                         |
    | VM_STATS_SCHEDULER | boolean | `true`        | Collect stats on the scheduler?                                                       |
    | VM_STATS_BASE_KEY  | string  | `"erlang.vm"` | All the VM stats will begin with this prefix (after the GLOBAL_PREFIX if that is set) |

## Use ##

1. List dogstatsd in your `rebar.config` file

```erlang
{deps, [
        {dogstatsd,     {git, "https://github.com/WhoopInc/dogstatsde.git"}}
]}.
```

2. List the dogstatsd application in your *.app.src file

3. Provide configuration as needed when starting up

4. For VM stats, no action is needed -- they'll collect on their own as long as the application is running

5. For custom metrics:

```erlang
dogstatsd:gauge("users.active", UserCount, #{ shard => ShardId, version > Vsn })
```

## Metric types ##

All metrics share the same signature:

```erlang
-type metric_name() :: iodata().
-type metric_value() :: number().
-type metric_sample_rate() :: number().
-type metric_tags() :: map().

-spec MetricFunction(metric_name(), metric_value(), metric_sample_rate(), metric_tags()) -> ok.
```

* The metric name is a string value with dots to separate levels of namespacing.
* The sample rate is a number between [0.0,1.0]. This is the probability of sending a particular metric.
* Tags are given as a map. The keys and values can be atoms, strings, or numbers.


Metric name and value are required. Sample rate defaults to 1.0. Tags defaults to an empty map.
