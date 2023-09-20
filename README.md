Active health checks and monitoring of Nginx upstreams
======================================================

[![Build Status](https://github.com/lyokha/nginx-healthcheck-plugin/workflows/CI/badge.svg)](https://github.com/lyokha/nginx-healthcheck-plugin/actions?query=workflow%3ACI)
[![Hackage](https://img.shields.io/hackage/v/ngx-export-healthcheck.svg?label=hackage%20%7C%20ngx-export-healthcheck&logo=haskell&logoColor=%239580D1)](https://hackage.haskell.org/package/ngx-export-healthcheck)

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
- [Nginx-based monitoring of normal upstreams](#nginx-based-monitoring-of-normal-upstreams)
    + [Normal upstreams, health checks and monitoring, related changes](#normal-upstreams-health-checks-and-monitoring-related-changes)
- [Periodic checks of healthy peers](#periodic-checks-of-healthy-peers)
- [Collecting Prometheus metrics](#collecting-prometheus-metrics)
    + [Normal upstreams, related changes](#normal-upstreams-related-changes)
    + [Shared upstreams, related changes](#shared-upstreams-related-changes)
- [Corner cases](#corner-cases)
- [Building and installation](#building-and-installation)

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

In all examples below, the shared library gets built from a simple Haskell
code which only exports service handlers from module *NgxExport.Healthcheck*.
The code is saved in file *ngx_healthcheck.hs*.

```haskell
module NgxHealthcheck where

import NgxExport.Healthcheck ()
```

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
                                              , epProto = Http
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
                                              , epProto = Http
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
service variables. Service keys also define values of header *Host* and the host
name for validation of server certificates used in health checks over *https*.
The host name gets known from a service key according to the following rule:

1. if the service key contains no *slashes*: the host name is equal to the
   service key,
2. otherwise, if the service key contains the only slash at its end: the host
   name is taken from the name of the server (without port) bound to the peer
   in the upstream configuration,
3. otherwise (if the service key contains slashes on the left of its end): the
   host name is equal to the part of the service key after the first slash from
   the left, for example, if a service key is equal to *1/healthcheck* then the
   host name is equal to *healthcheck*.

Besides time intervals in constructor *Conf*, a list of upstreams to check, an
optional *endpoint* and an optional monitoring port where statistics about
failed peers should be sent to are defined. Endpoints contain an URL on which
health checks will test failed peers, transport protocol (*Http* or *Https*),
and *passing rules* to describe in which cases failed peers must be regarded as
recovered. Currently, only two kinds of passing rules are supported:
*DefaultPassRule* (when a peer responds with *HTTP status 200*) and
*PassRuleByHttpStatus*. Internally, `updatePeers` calls a C function that reads
and updates Nginx data related to peer statuses.

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

In this *merged view*, all faulty peers are tagged with times of their latest
checks. Notice also that upstreams in the merged view never contain duplicate
peers.

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
                                              , epProto = Http
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
                                              , epProto = Http
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
                                              , epProto = Http
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
                                              , epProto = Http
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

Nginx-based monitoring of normal upstreams
------------------------------------------

Service *statsServer* is implemented using *Snap framework*. Basically, a native
Nginx implementation is not easy because the service must listen on a single
(not duplicated) file descriptor which is not the case when Nginx spawns more
than one worker processes. Running *statsServer* as a shared service is an
elegant solution as shared services guarantee that they occupy only one worker
at a time. However, *nginx-haskell-module* provides directive *single_listener*
which can be used to apply the required restriction in a custom Nginx virtual
server. This directive requires that the virtual server listens with option
*reuseport* and is only available on Linux with socket option
*SO_ATTACH_REUSEPORT_CBPF*.

Let's replace *statsServer* from the example with normal upstreams and
monitoring with an Nginx-based monitoring service using *single_listener* and
listening on port *8200*.

### Normal upstreams, health checks and monitoring, related changes

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
                                              , epProto = Http
                                              , epPassRule = DefaultPassRule
                                              }
              , sendStatsPort = Just 8200
              }
        ';

# ...


    haskell_run_service checkPeers $hs_service_healthcheck0
        'hs_service_healthcheck0
         Conf { upstreams     = ["u_backend"]
              , interval      = Sec 5
              , peerTimeout   = Sec 2
              , endpoint      = Just Endpoint { epUrl = "/healthcheck"
                                              , epProto = Http
                                              , epPassRule =
                                                      PassRuleByHttpStatus
                                                      [200, 404]
                                              }
              , sendStatsPort = Just 8200
              }
        ';

# ...

    haskell_var_empty_on_error $hs_stats;

# ...

        location /stat {
            allow 127.0.0.1;
            deny all;
            proxy_pass http://127.0.0.1:8200;
        }

# ...

    server {
        listen          8200 reuseport;
        server_name     stats;

        single_listener on;

        location /report {
            haskell_run_async_on_request_body receiveStats $hs_stats "Min 1";

            if ($hs_stats = '') {
                return 400;
            }

            return 200;
        }

        location /stat {
            haskell_async_content sendStats noarg;
        }

        location /stat/merge {
            haskell_async_content sendMergedStats noarg;
        }
    }
}
```

Handler *receiveStats* accepts a time interval corresponding to the value of
*ssPurgeInterval* from service *statsServer*. If the value is not readable (say,
*noarg*) then it is defaulted to *Min 5*.

Notice that the monitoring virtual server must listen on address *127.0.0.1*
because service *checkPeers* reports stats to this address.

Periodic checks of healthy peers
--------------------------------

The plugin checks periodically only faulty peers. Healthy peers become faulty
if they do not pass the standard Nginx rules, but this may happen only when
client requests trigger forwarding to the corresponding upstreams. Sometimes, it
makes sense to check healthy peers unconditionally. One way to achieve this is
running periodic active checks of the corresponding upstreams. For this, let's
use Haskell module
[*NgxExport.Tools.Subrequest*](https://hackage.haskell.org/package/ngx-export-tools-extra/docs/NgxExport-Tools-Subrequest.html).

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

Sadly, the subrequests may come to arbitrary worker processes which means that
there is no guarantee that faulty peers in *normal* upstreams will be checked in
all worker processes! The worker processes get chosen not only randomly but also
not fairly: a single worker process may serve all incoming requests during a
very long time. To make load between the workers more uniform, we can forward
the subrequests to some dedicated virtual server listening on a port with
*reuseport* and, additionally, randomize the hash for the *reuseport* by always
changing ports of the subrequests with HTTP request header *Connection: close*.

```nginx
    haskell_run_service simpleService_makeRequest $hs_check_u_backend
            '{"uri": "http://127.0.0.1:8011/Local/check/0/u_backend"}';

    haskell_run_service simpleService_makeRequest $hs_check_u_backend0
            '{"uri": "http://127.0.0.1:8011/Local/check/0/u_backend0"}';

    haskell_run_service simpleService_makeRequest $hs_check_u_backend1
            '{"uri": "http://127.0.0.1:8011/Local/check/0/u_backend1"}';
```

```nginx
    server {
        listen          8011 reuseport;
        server_name     aux_fair_load;

        location ~ ^/Local/check/0/(.+) {
            allow 127.0.0.1;
            deny all;
            haskell_run_async makeSubrequest $hs_subrequest
                    '{"uri": "http://127.0.0.1:8011/Local/check/1/$1"
                     ,"headers": [["Connection", "close"]]
                     }';
            return 200;
        }

        location ~ ^/Local/check/1/(.+) {
            allow 127.0.0.1;
            deny all;
            proxy_pass http://$1/healthcheck;
        }
    }
```

As soon as faulty servers from *normal* upstreams may still appear arbitrarily
in different worker processes, it makes sense to monitor them using the *merged
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

import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as ML
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Text.Encoding as T
import           Data.Binary
import           Data.Maybe

type MergedStats = MServiceKey AnnotatedFlatPeers
type SharedStats = MServiceKey FlatPeers
type FlatStats = MUpstream Int

toFlatStats :: MServiceKey a -> FlatStats
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

### Normal upstreams, related changes

```nginx
    haskell_run_service simpleService_prometheusConf $hs_prometheus_conf
            'PrometheusConf
                { pcMetrics = fromList
                    [("cnt_upstream_failure"
                      ,"Number of servers which are currently failed")
                    ]
                , pcGauges = fromList
                    ["cnt_upstream_failure@upstream=(u_backend)"
                    ,"cnt_upstream_failure@upstream=(u_backend0)"
                    ,"cnt_upstream_failure@upstream=(u_backend1)"
                    ]
                , pcScale1000 = fromList []
                }';

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
            haskell_async_content prometheusMetrics 
                    '["main", $cnt_collection, {}, {}]';
        }
```

### Shared upstreams, related changes

```nginx
    haskell_run_service simpleService_prometheusConf $hs_prometheus_conf
            'PrometheusConf
                { pcMetrics = fromList
                    [("cnt_upstream_failure"
                      ,"Number of servers which are currently failed")
                    ]
                , pcGauges = fromList
                    ["cnt_upstream_failure@upstream=(u_backend)"
                    ,"cnt_upstream_failure@upstream=(u_backend0)"
                    ,"cnt_upstream_failure@upstream=(u_backend1)"
                    ]
                , pcScale1000 = fromList []
                }';

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
            haskell_async_content prometheusMetrics
                    '["main", $cnt_collection, {}, {}]';
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

The plugin contains Haskell and C parts, and thus, it requires *ghc*, *Cabal*,
*gcc*, and a directory with the Nginx sources. The build tool also requires
[*patchelf*](https://github.com/NixOS/patchelf) and utility *nhm-tool* which is
shipped with package
[*ngx-export-distribution*](https://hackage.haskell.org/package/ngx-export-distribution).

Let's first install the Nginx module. For this, go to the directory with the
Nginx source code,

```ShellSession
$ cd /path/to/nginx/sources
```

compile,

```ShellSession
$ ./configure --add-dynamic-module=/path/to/nginx-healthcheck-plugin/sources
$ make modules
```

and install *ngx_healthcheck_plugin.so*.

```ShellSession
$ export NGX_HS_INSTALL_DIR=/var/lib/nginx
$ sudo install -d $NGX_HS_INSTALL_DIR
$ sudo cp objs/ngx_healthcheck_plugin.so $NGX_HS_INSTALL_DIR/libngx_healthcheck_plugin.so
```

Notice that we added prefix *lib* to the module's name!

Now let's build the Haskell code. For this, go to one of the directories with
Haskell handlers: *simple/*, *periodic/*, or *prometheus/*.

```ShellSession
$ cd -
$ cd simple
```

Before running *make*, tune the *constraints* stanza in *cabal.project*.
Currently, it should look similar to

```Cabal Config
constraints: ngx-export-healthcheck +snapstatsserver +healthcheckhttps
```

This line enforces building the Snap monitoring server and support for secure
connections to endpoints. To disable them, replace *+snapstatsserver* by
*-snapstatsserver* and *+healthcheckhttps* by *-healthcheckhttps*. To let Cabal
deduce whether to build these features automatically, remove the constraints.

Now run

```ShellSession
$ make PREFIX=$NGX_HS_INSTALL_DIR
$ sudo make PREFIX=$NGX_HS_INSTALL_DIR install
```

or simply

```ShellSession
$ make
$ sudo make install
```

if installation directory is */var/lib/nginx/*.

With ghc older than *9.0.1*, build with

```ShellSession
$ make LINKRTS=-lHSrts_thr-ghc$(ghc --numeric-version)
```

By default, package *ngx-export-healthcheck* gets installed from *Hackage*. To
build it locally, augment stanza *packages* inside *cabal.project* according to
the commentary attached to it.

