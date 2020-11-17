Active health checks and monitoring of Nginx upstreams
======================================================

**Disclaimer**: this is not an Nginx module in the traditional sense! It
compiles to a shared library that gets loaded in Nginx using directive
`haskell load` from Nginx module
[*nginx-haskell-module*](https://github.com/lyokha/nginx-haskell-module). Let's
call this *plugin*. The plugin provides support for active health checks and
monitoring of peers in normal *per-worker* and *shared* upstreams (those
declared with directive `zone`, possibly with dynamic peers), it supports all
kinds of balancing models present in Nginx (*round-robin*, *least_conn*, *hash*
and so forth). Both health checks and monitoring are optional, meaning that they
do not depend on each other, and one feature may be switched off while the other
is on.

Table of contents
-----------------

- [What the active health checks here means](#what-the-active-health-checks-here-means)
- [What the monitoring here means](#what-the-monitoring-here-means)
- [Examples](#examples)
    + [Normal upstreams, health checks and monitoring](#normal-upstreams-health-checks-and-monitoring)
    + [Normal upstreams, only health checks](#normal-upstreams-only-health-checks)
    + [Normal upstreams, only monitoring](#normal-upstreams-only-monitoring)
    + [Shared upstreams, health checks and monitoring](#shared-upstreams-health-checks-and-monitoring)
- [Periodic checks of healthy peers](#periodic-checks-of-healthy-peers)
- [Collecting Prometheus metrics](#collecting-prometheus-metrics)
- [Corner cases](#corner-cases)
- [Building and installation](#building-and-installation)
    + [Building module NgxExport.Healthcheck](#building-module-ngxexporthealthcheck)

What the active health checks here means
----------------------------------------

Well, they are not completely *active*. They are active only in one direction.
If all the peers in a checked upstream respond successfully then the active
health check does nothing! Successfulness of a peer response is checked against
the rules of directive `proxy_next_upstream` and settings in directive `server`.
This is quite standard in Nginx: a peer gets marked as *failed* when its
*N* responses do not pass the `proxy_next_upstream` rules during time period of
*T*. Values of *N* and *T* are defined in arguments *max_fails* and
*fail_timeout* in directive `server` respectively: by default they are equal to
*1* and *10s*. When a peer *fails*, it cannot be chosen for proxying requests
during the next time period of *T*.

And here the active health checks come into play! They merely move the time of
the last check of failed peers forward periodically, say, every 5 seconds, which
makes Nginx unable to return them back to the valid peers. So, do they keep
staying failed forever? No! The active health check tests them every those 5
seconds against its own rules and recover them when the check passes.

What the monitoring here means
------------------------------

There are traditional *per-worker* and *shared* upstreams. Monitoring of
traditional (or normal) upstreams is built in a dedicated *location* that
returns a JSON object which contains a list of *failed* peers nested in
**worker's PID / health check service / upstream** hierarchy. Monitoring of
shared upstreams is as well built in a dedicated location that returns a JSON
object with a list of failed peers nested in **health check service / upstream**
hierarchy. The meaning of the *health check service* will be explained later.

Examples
--------

### Normal upstreams, health checks and monitoring

```nginx
user                    nginx;
worker_processes        4;

events {
    worker_connections  1024;
}

http {
    default_type        application/octet-stream;
    sendfile            on;

    upstream u_backend {
        server localhost:8020 fail_timeout=600s;
        server localhost:8030 fail_timeout=600s;
    }

    upstream u_backend0 {
        server localhost:8040 fail_timeout=600s;
        server localhost:8050 fail_timeout=600s backup;
    }

    upstream u_backend1 {
        server localhost:8060 fail_timeout=600s;
        server localhost:8050 fail_timeout=600s backup;
    }

    proxy_next_upstream error timeout http_502;

    haskell load /var/lib/nginx/ngx_healthcheck.so;

    haskell_run_service checkPeers $hs_service_healthcheck
        'hs_service_healthcheck
         Conf { upstreams     = ["u_backend"
                                ,"u_backend0"
                                ,"u_backend1"
                                ]
              , interval      = Sec 5
              , peerTimeout   = Sec 2
              , endpoint      = Just Endpoint { epUrl = "/healthcheck"
                                              , epPassRule = DefaultPassRule
                                              }
              , sendStatsPort = Just 8100
              }
        ';

    haskell_service_update_hook updatePeers $hs_service_healthcheck;

    haskell_run_service checkPeers $hs_service_healthcheck0
        'hs_service_healthcheck0
         Conf { upstreams     = ["u_backend"]
              , interval      = Sec 5
              , peerTimeout   = Sec 2
              , endpoint      = Just Endpoint { epUrl = "/healthcheck"
                                              , epPassRule =
                                                      PassRuleByHttpStatus
                                                      [200, 404]
                                              }
              , sendStatsPort = Just 8100
              }
        ';

    haskell_service_update_hook updatePeers $hs_service_healthcheck0;

    haskell_run_service statsServer $hs_service_stats
        'StatsServerConf { ssPort          = 8100
                         , ssPurgeInterval = Min 5
                         }
        ';

    haskell_service_var_in_shm stats 64k /tmp $hs_service_stats;

    server {
        listen          8010;
        server_name     main;

        location /pass {
            proxy_pass http://u_backend;
        }

        location /pass0 {
            proxy_pass http://u_backend0;
        }

        location /pass1 {
            proxy_pass http://u_backend1;
        }

        location /stat {
            allow 127.0.0.1;
            deny all;
            proxy_pass http://127.0.0.1:8100;
        }
    }
}
```

In this configuration two *health check services* are declared: they are bound
to *service variables* `$hs_service_healthcheck` and `$hs_service_healthcheck0`.
The services run two instances of an *asynchronous* Haskell handler `checkPeers`
in every Nginx worker process when the workers start. The services have
complementary *service update hooks* that run a Haskell handler `updatePeers`
*synchronously* in the main Nginx thread when the service variable updates, i.e.
every 5 (or up to 7) seconds as stated in values *interval* and *peerTimeout* in
*constructor* *Conf*. Each service has an associated unique *key* specified
before the *Conf* declaration: in this example keys are equal to the names of
service variables. Besides time intervals, in constructor *Conf* a list of
upstreams to check, an optional *endpoint* and an optional monitoring port where
statistics about failed peers should be sent to are defined. Endpoints contain
an URL on which health checks will test failed peers, and *passing rules* to
describe in which cases failed peers must be regarded as valid again. Currently,
only two kinds of passing rules are supported: *DefaultPassRule* (when a peer
responds with *HTTP status 200*) and *PassRuleByHttpStatus*. Internally,
`updatePeers` calls a C function that reads and updates Nginx data related to
peer statuses.

Do not hesitate to declare as many Haskell services as you want, because they
are very cheap. When you have to check against a large number of upstreams, it
is even recommended to split services because this may potentially shorten
duration of the synchronous service hook.

Monitoring service *statsServer* collects statistics of failed peers. This is a
*shared* service because the associated service variable are declared as shared
in directive `haskell_service_var_in_shm`, and therefore it runs on a single
arbitrary Nginx worker all the time. Internally, it is implemented using [*Snap
framework*](http://snapframework.com/). The server is bound to the local IP
address *127.0.0.1* and only accessible from outside via a dedicated location
*/stat*. It will listen to incoming requests on the port specified in option
*ssPort*. Option *ssPurgeInterval* defines how often stats from died workers
will be purged.

Below is a typical response from the monitoring service.

```json
{
  "31493": [
    "2020-10-08T11:27:05.914756785Z",
    {
      "hs_service_healthcheck": {
        "u_backend": [
          "127.0.0.1:8020"
        ],
        "u_backend1": [
          "127.0.0.1:8060",
          "127.0.0.1:8050"
        ]
      },
      "hs_service_healthcheck0": {
        "u_backend": [
          "127.0.0.1:8020"
        ]
      }
    }
  ],
  "31494": [
    "2020-10-08T11:27:05.945503075Z",
    {
      "hs_service_healthcheck": {
        "u_backend": [
          "127.0.0.1:8020"
        ]
      },
      "hs_service_healthcheck0": {
        "u_backend": [
          "127.0.0.1:8020"
        ]
      }
    }
  ],
  "31496": [
    "2020-10-08T11:27:05.946501076Z",
    {}
  ],
  "31497": [
    "2020-10-08T11:27:05.932294049Z",
    {
      "hs_service_healthcheck": {
        "u_backend": [
          "127.0.0.1:8020"
        ]
      },
      "hs_service_healthcheck0": {
        "u_backend": [
          "127.0.0.1:8020"
        ]
      }
    }
  ]
}
```

When the monitoring service is accessed via URL */stat/merge*, its response data
gets merged from all worker processes.

```json
{
  "hs_service_healthcheck": {
    "u_backend": [
      [
        "2020-10-08T11:27:05.945503075Z",
        "127.0.0.1:8020"
      ]
    ],
    "u_backend1": [
      [
        "2020-10-08T11:27:05.914756785Z",
        "127.0.0.1:8060"
      ],
      [
        "2020-10-08T11:27:05.914756785Z",
        "127.0.0.1:8050"
      ]
    ]
  },
  "hs_service_healthcheck0": {
    "u_backend": [
      [
        "2020-10-08T11:27:05.945503075Z",
        "127.0.0.1:8020"
      ]
    ]
  }
}
```

In this *merged view*, all faulty servers are tagged with times of their latest
checks.

### Normal upstreams, only health checks

Henceforth I am going to skip unrelated parts of the configuration for brevity.

```nginx
    haskell_run_service checkPeers $hs_service_healthcheck
        'hs_service_healthcheck
         Conf { upstreams     = ["u_backend"
                                ,"u_backend0"
                                ,"u_backend1"
                                ]
              , interval      = Sec 5
              , peerTimeout   = Sec 2
              , endpoint      = Just Endpoint { epUrl = "/healthcheck"
                                              , epPassRule = DefaultPassRule
                                              }
              , sendStatsPort = Nothing
              }
        ';

    haskell_service_update_hook updatePeers $hs_service_healthcheck;

    haskell_run_service checkPeers $hs_service_healthcheck0
        'hs_service_healthcheck0
         Conf { upstreams     = ["u_backend"]
              , interval      = Sec 5
              , peerTimeout   = Sec 2
              , endpoint      = Just Endpoint { epUrl = "/healthcheck"
                                              , epPassRule =
                                                      PassRuleByHttpStatus
                                                      [200, 404]
                                              }
              , sendStatsPort = Nothing
              }
        ';

    server {
        listen          8010;
        server_name     main;

        location /pass {
            proxy_pass http://u_backend;
        }

        location /pass0 {
            proxy_pass http://u_backend0;
        }

        location /pass1 {
            proxy_pass http://u_backend1;
        }
    }
}
```

Values of *sendStatsPort* in both the health check services are not defined
(i.e. they are equal to special value *Nothing*), service *statsServer* and
location */stats* are removed.

### Normal upstreams, only monitoring

```nginx
    haskell_run_service checkPeers $hs_service_healthcheck
        'hs_service_healthcheck
         Conf { upstreams     = ["u_backend"
                                ,"u_backend0"
                                ,"u_backend1"
                                ]
              , interval      = Sec 5
              , peerTimeout   = Sec 2
              , endpoint      = Nothing
              , sendStatsPort = Just 8100
              }
        ';

    haskell_service_update_hook updatePeers $hs_service_healthcheck;

    haskell_run_service checkPeers $hs_service_healthcheck0
        'hs_service_healthcheck0
         Conf { upstreams     = ["u_backend"]
              , interval      = Sec 5
              , peerTimeout   = Sec 2
              , endpoint      = Nothing
              , sendStatsPort = Just 8100
              }
        ';

    haskell_run_service statsServer $hs_service_stats
        'StatsServerConf { ssPort          = 8100
                         , ssPurgeInterval = Min 5
                         }
        ';

    haskell_service_var_in_shm stats 64k /tmp $hs_service_stats;

    server {
        listen          8010;
        server_name     main;

        location /pass {
            proxy_pass http://u_backend;
        }

        location /pass0 {
            proxy_pass http://u_backend0;
        }

        location /pass1 {
            proxy_pass http://u_backend1;
        }

        location /stat {
            allow 127.0.0.1;
            deny all;
            proxy_pass http://127.0.0.1:8100;
        }
    }
}
```

Endpoints are not defined, the monitoring service and location */stat* are
intact.

### Shared upstreams, health checks and monitoring

```nginx
    upstream u_backend {
        zone u_backend 64k;
        server localhost:8020 fail_timeout=600s;
        server localhost:8030 fail_timeout=600s;
    }

    upstream u_backend0 {
        zone u_backend 64k;
        server localhost:8040 fail_timeout=600s;
        server localhost:8050 fail_timeout=600s backup;
    }

    upstream u_backend1 {
        zone u_backend 64k;
        server localhost:8060 fail_timeout=600s;
        server localhost:8050 fail_timeout=600s backup;
    }

    haskell_run_service checkPeers $hs_service_healthcheck
        'hs_service_healthcheck
         Conf { upstreams     = ["u_backend"
                                ,"u_backend0"
                                ,"u_backend1"
                                ]
              , interval      = Sec 5
              , peerTimeout   = Sec 2
              , endpoint      = Just Endpoint { epUrl = "/healthcheck"
                                              , epPassRule = DefaultPassRule
                                              }
              , sendStatsPort = Nothing
              }
        ';

    haskell_service_update_hook updatePeers $hs_service_healthcheck;

    haskell_run_service checkPeers $hs_service_healthcheck0
        'hs_service_healthcheck0
         Conf { upstreams     = ["u_backend"]
              , interval      = Sec 5
              , peerTimeout   = Sec 2
              , endpoint      = Just Endpoint { epUrl = "/healthcheck"
                                              , epPassRule =
                                                      PassRuleByHttpStatus
                                                      [200, 404]
                                              }
              , sendStatsPort = Nothing
              }
        ';

    haskell_service_update_hook updatePeers $hs_service_healthcheck0;

    haskell_service_var_in_shm upstreams 64k /tmp
            $hs_service_healthcheck $hs_service_healthcheck0;

    server {
        listen          8010;
        server_name     main;

        location /pass {
            proxy_pass http://u_backend;
        }

        location /pass0 {
            proxy_pass http://u_backend0;
        }

        location /pass1 {
            proxy_pass http://u_backend1;
        }

        location /stat {
            allow 127.0.0.1;
            deny all;
            haskell_async_content reportPeers;
        }
    }

```

The upstreams are now associated with a *shared memory zone*. The health check
services become *shared*, the monitoring service gets replaced with a simple
content handler `reportPeers` because peers are now shared between all Nginx
worker processes.

Combinations of *only health checks / only monitoring* are built trivially like
in the case of normal per-worker upstreams.

Below is a sample stats output for shared upstreams.

```json
{
  "hs_service_healthcheck": {
    "u_backend": [
      "127.0.0.1:8020"
    ],
    "u_backend1": [
      "127.0.0.1:8060",
      "127.0.0.1:8050"
    ]
  },
  "hs_service_healthcheck0": {
    "u_backend": [
      "127.0.0.1:8020"
    ]
  }
}
```

Periodic checks of healthy peers
--------------------------------

The plugin checks periodically only faulty peers. Healthy peers become faulty
if they do not pass the standard Nginx rules, but this may happen only when
client requests trigger forwarding to the corresponding upstreams. Sometimes, it
makes sense to check healthy peers unconditionally. One way to achieve this is
running periodic active checks of the corresponding upstreams. For this, let's
use Haskell module
[*NgxExport.Tools.Subrequest*](https://hackage.haskell.org/package/ngx-export-tools-extra/docs/NgxExport-Tools-Subrequest.html).

Here we have to mix the health checks and the *subrequest* functionality in a
single shared library, this means that we must use module
*NgxExport.Healthcheck* and build the library in the way described in the
[*dedicated section*](#building-module-ngxexporthealthcheck).

Below is the source code of the shared library.

```haskell
{-# LANGUAGE TemplateHaskell #-}

module NgxHealthcheckPeriodic where

import           NgxExport
import           NgxExport.Tools
import           NgxExport.Tools.Subrequest

import           NgxExport.Healthcheck ()

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L

makeRequest :: ByteString -> Bool -> IO L.ByteString
makeRequest = const . makeSubrequest

ngxExportSimpleService 'makeRequest $ PersistentService $ Just $ Sec 10
```

The periodic services must be declared in the Nginx configuration for all the
upstreams to check.

```nginx
    haskell_run_service simpleService_makeRequest $hs_check_u_backend
            '{"uri": "http://127.0.0.1:8010/Local/check/0/u_backend"}';

    haskell_run_service simpleService_makeRequest $hs_check_u_backend0
            '{"uri": "http://127.0.0.1:8010/Local/check/0/u_backend0"}';

    haskell_run_service simpleService_makeRequest $hs_check_u_backend1
            '{"uri": "http://127.0.0.1:8010/Local/check/0/u_backend1"}';
```

Sadly, a subrequest may come to an arbitrary worker process which means that
there is no guarantee that faulty peers in *normal* upstreams will be checked in
all workers processes! Therefore, it makes sense to share periodic check
services between all the workers by adding the corresponding service variables
into the list of directive *haskell_service_var_in_shm* for both normal and
shared upstreams.

Location */Local/check/0/* shall proxy to the specified upstream.

```nginx
        location ~ ^/Local/check/0/(.+) {
            allow 127.0.0.1;
            deny all;
            haskell_run_async makeSubrequest $hs_subrequest
                    '{"uri": "http://127.0.0.1:8010/Local/check/1/$1"}';
            return 200;
        }

        location ~ ^/Local/check/1/(.+) {
            allow 127.0.0.1;
            deny all;
            proxy_pass http://$1/healthcheck;
        }
```

The intermediate *makeSubrequest* catches possible *5xx* and other bad HTTP
statuses received from the upstream to prevent the periodic checks from throwing
exceptions. Additionally, checking the response status in the intermediate
location can be used for alerting that all servers in the upstream have failed.

As soon as faulty servers from *normal* upstreams may appear arbitrarily in
different worker processes, it makes sense to monitor them using the *merged
view*, i.e. via URL */stat/merge*.

Collecting Prometheus metrics
-----------------------------

With modules
[*NgxExport.Tools.Prometheus*](https://hackage.haskell.org/package/ngx-export-tools-extra/docs/NgxExport-Tools-Prometheus.html),
[*NgxExport.Tools.Subrequest*](https://hackage.haskell.org/package/ngx-export-tools-extra/docs/NgxExport-Tools-Subrequest.html),
and [*nginx-custom-counters-module*](https://github.com/lyokha/nginx-custom-counters-module),
custom Prometheus metrics can be collected. Let's monitor the number of
currently failed peers in all checked upstreams by extending examples from
section [*Examples*](#examples).

Below is the source code of the shared library.

```haskell
{-# LANGUAGE TemplateHaskell, TypeApplications, ViewPatterns #-}

module NgxHealthcheckPrometheus where

import           NgxExport
import           NgxExport.Tools
import           NgxExport.Tools.Prometheus ()
import           NgxExport.Tools.Subrequest ()

import           NgxExport.Healthcheck

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as ML
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Text.Encoding as T
import           Data.Binary
import           Data.Maybe
import           Data.Time.Clock

type MergedStats = ML.Map ServiceKey (Map Upstream [(UTCTime, Peer)])
type SharedStats = ML.Map ServiceKey Peers
type FlatStats = Map Upstream Int

toFlatStats :: ML.Map ServiceKey (Map Upstream [a]) -> FlatStats
toFlatStats = ML.foldr (flip $ M.foldrWithKey $
                           \k v -> M.alter (setN $ length v) k
                       ) M.empty
    where setN n Nothing = Just n
          setN n (Just m) = Just $ max n m

mergedFlatStats :: ByteString -> L.ByteString
mergedFlatStats =
    encode . toFlatStats . fromJust . readFromByteStringAsJSON @MergedStats

ngxExportYY 'mergedFlatStats

sharedFlatStats :: ByteString -> L.ByteString
sharedFlatStats =
    encode . toFlatStats . fromJust . readFromByteStringAsJSON @SharedStats

ngxExportYY 'sharedFlatStats

nFailedServers :: ByteString -> L.ByteString
nFailedServers v =
    let (T.decodeUtf8 -> u, decode @FlatStats . L.fromStrict . C8.tail -> s) =
            C8.break (== '|') v
    in C8L.pack $ show $ fromMaybe 0 $ M.lookup u s

ngxExportYY 'nFailedServers
```

### Normal upstreams (related changes)

```nginx
    haskell_run_service simpleService_prometheusConf $hs_prometheus_conf
            'PrometheusConf
                { pcMetrics = fromList
                    [("cnt_upstream_failure"
                      ,"Number of servers which are currently failed")
                    ]
                , pcGauges = ["cnt_upstream_failure@upstream=(u_backend)"
                             ,"cnt_upstream_failure@upstream=(u_backend0)"
                             ,"cnt_upstream_failure@upstream=(u_backend1)"
                             ]
                , pcScale1000 = []
                }';

    haskell_var_empty_on_error $hs_prom_metrics;

# ...

        location /stat {
            allow 127.0.0.1;
            deny all;
            proxy_pass http://127.0.0.1:8100;
        }

        location /stat/merge {
            allow 127.0.0.1;
            deny all;

            counter $cnt_upstream_failure@upstream=(u_backend)
                    set $hs_n_u_backend;
            counter $cnt_upstream_failure@upstream=(u_backend0)
                    set $hs_n_u_backend0;
            counter $cnt_upstream_failure@upstream=(u_backend1)
                    set $hs_n_u_backend1;

            haskell_run_async makeSubrequestFull $hs_subrequest
                    '{"uri": "http://127.0.0.1:8100/stat/merge"}';
            haskell_run extractBodyFromFullResponse $hs_subrequest_body
                    $hs_subrequest;
            haskell_run mergedFlatStats $hs_failed_backends
                    $hs_subrequest_body;
            haskell_run nFailedServers $hs_n_u_backend
                    u_backend|$hs_failed_backends;
            haskell_run nFailedServers $hs_n_u_backend0
                    u_backend0|$hs_failed_backends;
            haskell_run nFailedServers $hs_n_u_backend1
                    u_backend1|$hs_failed_backends;

            haskell_content fromFullResponse $hs_subrequest;
        }

        location /metrics {
            allow 127.0.0.1;
            deny all;

            haskell_run_async makeSubrequest $hs_subrequest
                    '{"uri": "http://127.0.0.1:8010/stat/merge"}';
            haskell_run toPrometheusMetrics $hs_prom_metrics
                    '["main", $cnt_collection, {}, {}]';

            if ($hs_prom_metrics = '') {
                return 503;
            }

            default_type "text/plain; version=0.0.4; charset=utf-8";

            echo -n $hs_prom_metrics;
        }
```

### Shared upstreams (related changes)

```nginx
    haskell_run_service simpleService_prometheusConf $hs_prometheus_conf
            'PrometheusConf
                { pcMetrics = fromList
                    [("cnt_upstream_failure"
                      ,"Number of servers which are currently failed")
                    ]
                , pcGauges = ["cnt_upstream_failure@upstream=(u_backend)"
                             ,"cnt_upstream_failure@upstream=(u_backend0)"
                             ,"cnt_upstream_failure@upstream=(u_backend1)"
                             ]
                , pcScale1000 = []
                }';

    haskell_var_empty_on_error $hs_prom_metrics;

# ...

        location /stat {
            allow 127.0.0.1;
            deny all;
            haskell_async_content reportPeers;
        }

        location /stat/shared {
            allow 127.0.0.1;
            deny all;

            counter $cnt_upstream_failure@upstream=(u_backend)
                    set $hs_n_u_backend;
            counter $cnt_upstream_failure@upstream=(u_backend0)
                    set $hs_n_u_backend0;
            counter $cnt_upstream_failure@upstream=(u_backend1)
                    set $hs_n_u_backend1;

            haskell_run_async makeSubrequestFull $hs_subrequest
                    '{"uri": "http://127.0.0.1:8010/stat"}';
            haskell_run extractBodyFromFullResponse $hs_subrequest_body
                    $hs_subrequest;
            haskell_run sharedFlatStats $hs_failed_backends
                    $hs_subrequest_body;
            haskell_run nFailedServers $hs_n_u_backend
                    u_backend|$hs_failed_backends;
            haskell_run nFailedServers $hs_n_u_backend0
                    u_backend0|$hs_failed_backends;
            haskell_run nFailedServers $hs_n_u_backend1
                    u_backend1|$hs_failed_backends;

            haskell_content fromFullResponse $hs_subrequest;
        }

        location /metrics {
            allow 127.0.0.1;
            deny all;

            haskell_run_async makeSubrequest $hs_subrequest
                    '{"uri": "http://127.0.0.1:8010/stat/shared"}';
            haskell_run toPrometheusMetrics $hs_prom_metrics
                    '["main", $cnt_collection, {}, {}]';

            if ($hs_prom_metrics = '') {
                return 503;
            }

            default_type "text/plain; version=0.0.4; charset=utf-8";

            echo -n $hs_prom_metrics;
        }
```

Below is a typical sample of the Prometheus metrics.

```ShellSession
# HELP cnt_upstream_failure Number of servers which are currently failed
# TYPE cnt_upstream_failure gauge
cnt_upstream_failure{upstream="u_backend"} 2.0
cnt_upstream_failure{upstream="u_backend0"} 0.0
cnt_upstream_failure{upstream="u_backend1"} 0.0
```

Corner cases
------------

- In Nginx before version *1.12.0*, there was a so-called *quick recovery*
  mechanism, meaning that when all peers in the main or the backup part of an
  upstream get failed, then they immediately were recovered. As such, health
  checks are impossible in such a case.

- When the main or the backup part of an upstream contains only one peer, then
  the peer never fails. For the sake of the health checks, such an upstream
  could be extended by a fake server marked as *down*, for example

    ```nginx
        upstream u_backend {
            server localhost:8020;
            server localhost:8020 down;
        }
    ```

  But there is a better solution proposed in Nginx module
  [*nginx-combined-upstreams-module*](https://github.com/lyokha/nginx-combined-upstreams-module).

    ```nginx
        upstream u_backend {
            server localhost:8020;
            extend_single_peers;
        }
    ```

  Directive `extend_single_peers` adds a fake peer only when an upstream part
  (the main or the backup) contains a single peer.

Building and installation
-------------------------

The plugin contains Haskell and C parts, and thus it requires *ghc*, *cabal*,
*gcc*, and a directory with the Nginx sources. The trickiest part is building
the C plugin. There are two options for that: static compilation and linkage
against the resulting shared library, and building an Nginx dynamic module.

For the first build option, environment variable *NGX_HOME* with the directory
where the Nginx sources are located must be set.

```ShellSession
$ export NGX_HOME=/path/to/Nginx/sources
```

Then run *cabal update* (optionally, when the list of Haskell packages has never
been built or is too old), and *make*.

```ShellSession
$ cabal update
$ make
```

For the second build option, go to the directory with the Nginx sources, run
*configure* with option
*--add-dynamic-module=/path/to/nginx-healthcheck-plugin/sources*, then *make
modules* and copy the built library to some directory where this can be loaded
by the plugin in the run-time (let this directory be */var/lib/nginx/hslibs/*,
you may need to patch the resulting shared library with *make patchlib*, see
below).

```ShellSession
$ ./configure --add-dynamic-module=/path/to/nginx-healthcheck-plugin/sources
$ make modules
```

Copy the plugin into the target directory being a superuser.

```ShellSession
# cp objs/ngx_healthcheck_plugin.so /var/lib/nginx/hslibs/libngx_healthcheck_plugin.so
```

Notice that we added prefix *lib* to the module's name!

Then go back to this plugin's directory, set environment variable
*NGX_MODULE_PATH*, and build the plugin just like in the first build option.

```ShellSession
$ export NGX_MODULE_PATH=/var/lib/nginx/hslibs
$ cabal update
$ make
```

This must create a cabal sandbox, then build all dependencies in it, compile the
C plugin (if the first build option was chosen), and finally compile and link
shared library *ngx_healthcheck.so*. If all went well, and the target machine is
the same as the builder, then you may copy the library to the directory that is
specified in directive `haskell load`, i.e. */var/lib/nginx/*. However, in many
cases this simple approach won't work smoothly because of lack of access rights
to the sandbox directories from the Nginx workers' owner (normally, *nginx* or
*nobody*). Additionally, these directories are supposed to be deleted when
running *make clean*.

A better approach is to collect all dependent Haskell libraries in a single
directory. This approach suits well for remote deployment, even if the target
system does not have *ghc* installed.

Run

```ShellSession
$ make hslibs
```

and look into directory *hslibs/*: it should contain many shared libraries
*ngx_healthcheck.so* depends on. The libraries must be copied to the target
machine in a directory the dynamic linker is aware of. If the target directory
is not known to the linker (say, you want to install the libraries into
directory */var/lib/nginx/hslibs/*), then *ngx_healthcheck.so* must be patched
to provide corresponding *runpath*.

```ShellSession
$ export HSLIBS_INSTALL_DIR=/var/lib/nginx/hslibs
$ make patchlib
```

This makes use of utility [*patchelf*](https://nixos.org/patchelf.html), so the
latter must be available in your system.

When built objects, collected libraries, and the cabal sandbox are no longer
needed, run

```ShellSession
$ make clean
```

to delete them all. Or run

```ShellSession
$ make lenient-clean
```

to delete all except the sandbox.

### Building module NgxExport.Healthcheck

When building a custom Haskell library with the health check functionality, a
separate Haskell health check module is required. It is easy to build and
install the module with command

```ShellSession
$ cabal --ignore-sandbox v1-install
```

(you may prefer the *new-style* cabal command *v2-install*). Then, in the custom
library, import the module.

```haskell
import NgxExport.Healthcheck ()
```

When building the shared library, link it against the C code. The easiest way to
achieve this goal is using the second build option with variable
*NGX_MODULE_PATH* as it was explained previously. Let the name of the source
file of the shared library be *custom.hs*.

```ShellSession
$ ghc -Wall -O2 -dynamic -shared -fPIC -lHSrts_thr-ghc$(ghc --numeric-version) -L$NGX_MODULE_PATH -lngx_healthcheck_plugin custom.hs -o custom.so -fforce-recomp
```

It's time to collect all dependent libraries, patch *custom.so*, and install
everything: they are the same steps as when doing *make hslibs* and *make
patchlib*, but this time we are going to use utility
[*hslibdeps*](https://github.com/lyokha/nginx-haskell-module/blob/master/utils/README.md#utility-hslibdeps).

```ShellSession
$ hslibdeps -t $HSLIBS_INSTALL_DIR custom.so
```

If values of *HSLIBS_INSTALL_DIR* and *NGX_MODULE_PATH* differ then the second
path must be added too.

```ShellSession
$ hslibdeps -t $NGX_MODULE_PATH custom.so
```

Copy library *custom.so* into directory */var/lib/nginx/* (this must correspond
to the directory specified in Nginx directive *haskell load*) being a superuser.

```ShellSession
# cp custom.so /var/lib/nginx
```

Then copy all dependent Haskell libraries into the target directory.

```ShellSession
# cp -v .hslibs/* $HSLIBS_INSTALL_DIR
```

