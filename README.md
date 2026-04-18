# ppad-bolt9

[![](https://img.shields.io/hackage/v/ppad-bolt9?color=blue)](https://hackage.haskell.org/package/ppad-bolt9)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-bolt9-lightblue)](https://docs.ppad.tech/bolt9)

A Haskell implementation of Lightning Network feature flags (BOLT #9),
providing type-safe feature vectors with validation and wire format
encoding.

## Overview

BOLT #9 defines feature flags that Lightning nodes advertise to indicate
support for optional protocol features. Features are represented as bit
positions in a variable-length bit vector:

- **Even bits** (0, 2, 4, ...) indicate *required* support
- **Odd bits** (1, 3, 5, ...) indicate *optional* support

For example, `basic_mpp` (multi-part payments) uses bits 16/17. A node
setting bit 16 requires peers to support it; bit 17 indicates optional
support.

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  > import Lightning.Protocol.BOLT9
  > import Data.Maybe (fromJust)
  >
  > -- Look up features by name
  > let mpp = fromJust $ featureByName "basic_mpp"
  > featureBaseBit mpp
  16
  > featureDependencies mpp
  ["payment_secret"]
  >
  > -- Build a feature vector with optional basic_mpp support
  > let fv = setFeature mpp False empty
  > hasFeature mpp fv
  Just False
  >
  > -- Validation catches missing dependencies
  > validateLocal Init fv
  Left [MissingDependency "basic_mpp" "payment_secret"]
  >
  > -- Add the dependency
  > let ps = fromJust $ featureByName "payment_secret"
  > let fv' = setFeature ps False $ setFeature mpp False empty
  > validateLocal Init fv'
  Right ()
  >
  > -- Render to wire format (compact, no leading zeros)
  > render fv'
  "\STX\128"
  >
  > -- Parse from wire format
  > let fv'' = parse "\STX\128"
  > listFeatures fv''
  [(Feature {featureName = "payment_secret", ...},False),
   (Feature {featureName = "basic_mpp", ...},False)]
  >
  > -- Remote validation: unknown optional bits are OK
  > validateRemote Init (setBit 201 empty)
  Right ()
  >
  > -- But unknown required bits are rejected
  > validateRemote Init (setBit 200 empty)
  Left [UnknownRequiredBit 200]
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

This is a pre-release library and makes no claims about security whatsoever.

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
