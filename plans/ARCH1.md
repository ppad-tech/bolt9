# ARCH1: High-level architecture for ppad-bolt9

## Goals

- Represent BOLT 9 feature bits with strong types and safe invariants.
- Provide total parsing, validation, and construction of feature vectors.
- Enforce dependencies, context restrictions, and optional/required pairing.
- Keep the core API small and stable; push policy to explicit validators.

## Core concepts

- Feature bit: numbered bit (0-based) with optional/required pairing
  (odd/optional, even/required).
- Feature pair: optional bit N and required bit N-1 (for odd N).
- Context: where a feature vector appears (init, node_announcement,
  channel_announcement, invoice, blinded path, channel_type).
- Feature vector: packed bitset with context-specific restrictions.

## Module layout (proposed)

- `Lightning.Protocol.BOLT9`
  - Public API: feature types, parsing, rendering, validation helpers.
  - Re-export safe constructors and lookup functions.

- `Lightning.Protocol.BOLT9.Feature`
  - Enumerates known features from 09-features.md.
  - Tables: bit number, name, contexts, dependencies, assumed.
  - Total lookup: bit -> known feature info (Maybe).

- `Lightning.Protocol.BOLT9.Context`
  - ADT for contexts: Init, NodeAnn, ChanAnn, Invoice, Blinded, ChanType.
  - Helpers for context-specific rules (C-, C+ handling).

- `Lightning.Protocol.BOLT9.Vector`
  - Packed representation of feature bits (ByteString).
  - Functions: empty, set, clear, member, union, normalize.
  - Safe constructors for optional vs required bits.

- `Lightning.Protocol.BOLT9.Validate`
  - Validation of a vector against:
    - context restrictions
    - optional/required conflict
    - dependency closure
    - must-not-set unknown bits (if required by caller)
  - Validation of remote vectors for unknown bits handling.

## Data model notes

- Use newtypes for bit indices and for optional vs required bits.
- Encode odd/even constraints in constructors (smart constructors).
- Store dependencies as feature pairs (logical dependencies) rather than
  raw bit numbers, then expand to actual bits for validation.

## Invariants to enforce

- No vector may contain both optional and required bits for a feature.
- Context restrictions are respected:
  - Some bits only in Init/Node/Chan/Invoice/Blinded/ChanType.
  - C-: only optional (odd bit) allowed in channel_announcement.
  - C+: only required (even bit) allowed in channel_announcement.
- Known dependencies are transitively satisfied.
- Unknown bits may be allowed but must be treated as mandatory if
  both optional and required are set (receiver rule).

## API shape

- Parsing and rendering:
  - parseFeatureVector :: ByteString -> Either Error FeatureVector
  - renderFeatureVector :: FeatureVector -> ByteString

- Construction:
  - emptyVector
  - setOptional :: Feature -> FeatureVector -> FeatureVector
  - setRequired :: Feature -> FeatureVector -> FeatureVector

- Validation:
  - validateLocal :: Context -> FeatureVector -> Either Error Validated
  - validateRemote :: Context -> FeatureVector -> Either Error Validated

## Validation policy

- Local validation is strict: unknown bits are rejected by default.
- Remote validation is configurable:
  - accept unknown optional bits
  - reject unknown required bits
  - configurable fallback for unknown pairs

## Testing strategy

- Unit tests for all known features (bit numbers, contexts, deps).
- Property tests:
  - dependencies are closed under set operations
  - no constructor can produce invalid parity
  - render/parse roundtrip

