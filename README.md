# ppad-bolt9

A Haskell implementation of Lightning Network feature flags (BOLT #9).

## Usage

A sample GHCi session:

```
  > import Lightning.Protocol.BOLT9
  > -- TODO
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/bolt9](https://docs.ppad.tech/bolt9).

## Performance

The aim is best-in-class performance for feature flag handling.
Benchmarks are available under `bench/` and can be run with:

```
$ cabal bench
```

## Security

This library targets safe, validated handling of Lightning BOLT #9
feature flags. If you discover any vulnerabilities, please disclose
them via security@ppad.tech.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-bolt9
```

to get a REPL for the main library.

## Attribution

This library implements the Lightning Network BOLT #9 specification:
https://github.com/lightning/bolts/blob/master/09-features.md

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
