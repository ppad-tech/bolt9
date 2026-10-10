# ppad-bolt9

[![](https://img.shields.io/hackage/v/ppad-bolt9?color=blue)](https://hackage.haskell.org/package/ppad-bolt9)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-bolt9-lightblue)](https://docs.ppad.tech/bolt9)

A pure Haskell implementation of Lightning Network feature flags, per
[BOLT #9][bolt9]: an abstract feature vector type that preserves its
wire bytes, the table of known features with their contexts and
dependencies, sender- and receiver-side validation for each context,
and BOLT #2 channel types.

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  >
  > -- import qualified
  > import qualified Lightning.Protocol.BOLT9 as BOLT9
  >
  > -- build a feature vector for an init message, with dependencies
  > let opt = BOLT9.Optional
  > let fv = BOLT9.set_feature_with_deps BOLT9.OptionZeroconf opt BOLT9.empty
  > BOLT9.list_features fv
  [(OptionScidAlias,Optional),(OptionZeroconf,Optional)]
  >
  > -- check it against the sender's rules, then encode it
  > BOLT9.validate_local BOLT9.InitContext fv
  Right ()
  > BOLT9.render fv
  "\b\128\NUL\NUL\NUL\NUL\NUL"
  >
  > -- a received vector keeps its exact bytes
  > let theirs = BOLT9.parse "\NUL\STX\NUL\NUL"
  > BOLT9.render theirs
  "\NUL\STX\NUL\NUL"
  > BOLT9.test_feature BOLT9.BasicMpp theirs
  Just Optional
  >
  > -- check it against the receiver's rules for init
  > BOLT9.validate_remote BOLT9.InitContext theirs
  Right ()
  > BOLT9.validate_remote BOLT9.InitContext (BOLT9.set_bit 100 theirs)
  Left (UnknownBit 100 :| [])
  >
  > -- channel types follow BOLT #2
  > let ct = BOLT9.ChannelType BOLT9.BasicAnchors False True
  > BOLT9.channel_type (BOLT9.channel_type_features ct) == Just ct
  True
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/bolt9](https://docs.ppad.tech/bolt9).

## Specification

The library implements BOLT #9 as of [lightning/bolts@1aadb719][spec]
(2026-09-21), a copy of which lives in `etc/09-features.md`, together
with the feature-vector rules of BOLTs #1, #2, #4, #7 and #11 for each
context.

Not implemented:

* Feature fields of BOLT #12 offers, invoice requests and invoices,
  which the BOLT #9 table assigns no context.
* The BOLT #11 encoding of features as 5-bit groups.
* BOLT #1's notions of *offered* and *negotiated* features.
* BOLT #2's rule against `option_scid_alias` in the channel type of an
  announced channel, which depends on `channel_flags`.
* BOLT #4's check that a payment uses only features in
  `allowed_features`.
* The `C-` and `C+` presentation rules for `channel_announcement`. No
  feature in the table uses them, or `C`.
* The receiving requirements specific to each feature, which BOLT #9
  defers to the linked sections of the other BOLTs.

BOLT #2 defines `zero_fee_commitments` as a basic channel type, though
the BOLT #9 table doesn't list it in the `T` context; `ChannelType`
follows BOLT #2.

## Performance

The aim is best-in-class performance for pure Haskell code. Run the
benchmark suite with:

```
$ cabal bench
```

## Security

This is a pre-release library and makes no claims about security
whatsoever.

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

[bolt9]: https://github.com/lightning/bolts/blob/master/09-features.md
[spec]: https://github.com/lightning/bolts/blob/1aadb719b4007c4cea0ba6e36b08c4fb53788dee/09-features.md
[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
