### 1.6.1

- Refactored support for *https* transport protocol.
  + **Breaking changes**: constructor *Https* gets a new parameter *server
    name*, see details [*here*](https://github.com/lyokha/nginx-healthcheck-plugin#secure-connection-to-endpoints-via-https).
- Request header *Host* now contains
  + *in case of Http*: the name of the server from the upstream configuration
    associated with the peer,
  + *in case of Https*: the configured server name.

### 1.6

- Added support for *https* transport protocol.
  + **Breaking changes**: endpoint configuration gets a new field *epProto*,
    see updated examples in the project's *README.md*.
- A new approach to building custom handlers with Cabal Nix-style local builds
  and [*ngx-export-distribution*](https://hackage.haskell.org/package/ngx-export-distribution),
  see details in the project's *simple/*, *periodic/*, and *prometheus/*
  subdirectories.
- Improvement: using a single http manager for all service keys.

### 1.5

- Refactoring of type imports.

