### 1.6

- Added support for *https* transport protocol.
  + **Breaking changes**: endpoint configuration gets a new field *epProto*,
    see updated examples in the project's *README.md*.
- A new approach to building custom handlers with Cabal Nix-style local builds
  and [*ngx-export-distribution*](https://hackage.haskell.org/package/ngx-export-distribution),
  see details in the project's *simple/*, *periodic/*, and *prometheus/*
  subdirectories.

### 1.5

- Refactoring of type imports.

