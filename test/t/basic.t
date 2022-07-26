# vi:filetype=

use Test::Nginx::Socket;

repeat_each(1);
plan tests => repeat_each() * (2 * blocks()) - 1;

no_shuffle();
run_tests();

__DATA__

=== TEST 1: pass
--- http_config
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

    haskell load $TEST_NGINX_HCLIB_DIR/ngx_healthcheck.so;

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
              , sendStatsPort = Just 8200
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
              , sendStatsPort = Just 8200
              }
        ';

    haskell_service_update_hook updatePeers $hs_service_healthcheck0;

    haskell_var_empty_on_error $hs_stats;

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
--- config
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
            proxy_pass http://127.0.0.1:8200;
        }
--- request
    GET /pass
--- wait: 8
--- error_code: 502

=== TEST 2: stat
--- request
    GET /stat
--- response_body_like: ^\{"\d+":\["[^"]+Z",\{"hs_service_healthcheck":\{"u_backend":\[(?:"127.0.0.1:8020",){1,2}(?:"127.0.0.1:8030",?){1,2}\]\},"hs_service_healthcheck0":\{"u_backend":\[(?:"127.0.0.1:8020",){1,2}(?:"127.0.0.1:8030",?){1,2}\]\}\}\]\}$
--- error_code: 200

