Active health checks and monitoring for Nginx upstreams
=======================================================

**Disclaimer**: this is not an Nginx module in the traditional sense! It
compiles to a shared library that gets loaded in Nginx using directive
`haskell_run_service` from Nginx module
[*nginx-haskell-module*](https://github.com/lyokha/nginx-haskell-module). Let's
call this *plugin*. The plugin provides support for active health checks and
monitoring of peers in normal *per-worker* and *shared* upstreams (those
declared with directive `zone`, possibly with dynamic peers), it supports all
kinds of balancing models present in Nginx (*round-robin*, *least_conn*, *hash*
and so forth). Both health checks and monitoring are optional, meaning that they
do not depend on each other, and one feature may be switched off while the other
is on.

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
the last check of failed peers forward periodically, say every 5 seconds, which
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
in every Nginx worker process when the workers start. The services has
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
  "19040": [
    "2018-06-14T20:02:18Z",
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
  "19042": [
    "2018-06-14T20:02:18Z",
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
  "19043": [
    "2018-06-14T20:02:18Z",
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
  "19045": [
    "2018-06-14T20:02:18Z",
    {}
  ]
}
```

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

This is not straightforward yet. The plugin contains Haskell and C parts, and
thus requires *ghc*, *cabal*, *gcc*, and a directory with Nginx sources.
Basically, you need to export this Nginx home directory like

```ShellSession
$ export NGX_HOME=/path/to/Nginx/sources
```

and then run *cabal update* (optionally, when the list of Haskell packages has
never been built or is too old), and *make*.

```ShellSession
$ cabal update
$ make
```

This must create a cabal sandbox, then build all dependencies in it, compile C
plugin, and finally compile and link shared library *ngx_healthcheck.so*. If all
went well, and the target machine is the same as the builder, then you may copy
the library to the directory that is specified in directive `haskell load`, i.e.
*/var/lib/nginx/*. However in many cases this simple approach won't work
smoothly because of lack of access rights to the sandbox directories from the
Nginx workers' owner (normally, *nginx*). Additionally, these directories are
supposed to be deleted when running `make clean`.

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
is not known to the linker (say you want to install the libraries into directory
*/var/lib/nginx/hslibs/*), then *ngx_healthcheck.so* must be patched to provide
corresponding *runpath*.

```ShellSession
$ export HSLIBS_INSTALL_DIR=/var/lib/nginx/hslibs
$ make patchlib
```

This makes use of utility [*patchelf*](https://nixos.org/patchelf.html), so the
latter must be available in your system.

When built objects, collected libraries, and the cabal sandbox are no longer
needed run

```ShellSession
$ make clean
```

to delete them all. Or run

```ShellSession
$ make lenient-clean
```

to delete all except the sandbox.

