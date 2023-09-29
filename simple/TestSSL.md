To test health checks over SSL, SSL certificates must be generated.

Let's first create subdirectories to store them.

```ShellSession
$ mkdir -p certs/root certs/server
```

Then generate root and server certificates.

```ShellSession
$ openssl genrsa -out certs/root/rootCA.key 2048
$ openssl req -new -x509 -days 365 -key certs/root/rootCA.key -out certs/root/rootCA.crt -subj '/C=US/ST=California/L=San Francisco/O=My Company/OU=Healthcheck Test/CN=My Company Root/UID=healthcheck'
$ openssl req -new -newkey rsa:2048 -nodes -out certs/server/server.csr -subj '/C=US/ST=California/L=San Francisco/O=My Company/OU=Healthcheck Test/CN=localhost/UID=healthcheck' -keyout certs/server/server.key
```

You may use other subject values.

Now put OpenSSL configuration in directory *certs/*.

```INI
authorityKeyIdentifier = keyid, issuer
basicConstraints = CA:FALSE
keyUsage = digitalSignature, nonRepudiation, keyEncipherment, dataEncipherment
subjectAltName = @alt_names
[alt_names]
DNS.1 = localhost
```

Sign certificates.

```ShellSession
$ openssl x509 -req -days 365 -in certs/server/server.csr -CA certs/root/rootCA.crt -CAkey certs/root/rootCA.key -set_serial 01 -out certs/server/server.crt -extfile certs/openssl.cnf
```

Make them trusted by the system (the following commands have meaning in
*Fedora*, other systems may require other commands).

```ShellSession
$ sudo trust anchor --store certs/server/server.crt
$ sudo trust anchor --store certs/root/rootCA.crt
$ sudo update-ca-trust
```

Enable SSL in *backends.conf*.

```nginx
    server {
        listen          8030 ssl;
        server_name     localhost;

        ssl_certificate      /absolute/path/to/certs/server/server.crt;
        ssl_certificate_key  /absolute/path/to/certs/server/server.key;
        ssl_protocols        TLSv1 TLSv1.1 TLSv1.2 TLSv1.3;
        ssl_ciphers          HIGH:!aNULL:!MD5;
```

Or run an OpenSSL server.

```ShellSession
$ openssl s_server -key certs/server/server.key -cert certs/server/server.crt -port 8030 -www
```

Make sure that fields *epProto* in *nginx.conf* are set to *Https* and service
keys end by slashes.

```nginx
    haskell_run_service checkPeers $hs_service_healthcheck
        'hs_service_healthcheck/
         Conf { upstreams     = ["u_backend"
                                ,"u_backend0"
                                ,"u_backend1"
                                ]
              , interval      = Sec 5
              , peerTimeout   = Sec 2
              , endpoint      = Just Endpoint { epUrl = "/healthcheck"
                                              , epProto = Https
                                              , epPassRule = DefaultPassRule
                                              }
              , sendStatsPort = Just 8100
              }
        ';
```

