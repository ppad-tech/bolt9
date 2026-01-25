# IMPL1: Implementation plan for ppad-bolt9

## Scope

Implement BOLT 9 feature flags with typed representation, parsing,
validation, and tests driven by etc/09-features.md.

## Step plan

1) Baseline types and context model
   - Add `Context` ADT and helpers for context rules (C-/C+).
   - Add newtypes for `BitIndex`, `OptionalBit`, `RequiredBit` with
     smart constructors enforcing odd/even parity.
   - Define `FeatureVector` wrapper over strict ByteString with basic
     operations (empty, set, clear, member).
   - Independent: yes, can proceed in parallel with step 2.

2) Known feature table
   - Encode the known features from 09-features.md in a table with:
     name, base bit, contexts, dependencies, assumed.
   - Provide total lookup functions:
     `featureByBit`, `featureByName`.
   - Provide accessors for contexts and dependencies.
   - Independent: yes, can proceed in parallel with step 1.

3) Parsing and rendering
   - Implement parse from ByteString into FeatureVector (bit order per
     BOLT 1, least-significant bit is bit 0).
   - Implement rendering back to ByteString preserving compact length
     rules (trim leading zero bytes).
   - Add total helpers to map between bits and features.
   - Depends on steps 1-2.

4) Validation engine
   - Implement local validation:
     - no optional+required pair set
     - context restrictions (C-/C+ and allowed contexts)
     - dependency closure (transitive)
     - must set required bits for required features
   - Implement remote validation:
     - treat unknown optional bits as ignorable
     - reject unknown required bits
     - if both bits set, treat as required
   - Provide error ADT with structured reasons.
   - Depends on steps 1-3.

5) Public API surface in Lightning.Protocol.BOLT9
   - Expose safe constructors and validation helpers.
   - Add Haddock examples for exported functions.
   - Ensure module header and OPTIONS_HADDOCK prune.
   - Depends on steps 1-4.

6) Tests
   - Unit tests for known features table and bit parity.
   - Unit tests for context restrictions and dependency checks.
   - Property tests for render/parse roundtrip and dependency closure.
   - Depends on steps 1-4.

7) Benchmarks
   - Criterion benchmarks for parse/validate/render.
   - Weigh benchmark for allocation profile.
   - Define NFData instances as needed.
   - Depends on steps 1-4.

## Notes for subagents

- Use strictness annotations and INLINE for small helpers.
- Avoid partial functions; make pattern matches exhaustive.
- Keep lines under 80 chars.
- Do not add new external deps without approval.
- If you introduce validation policy knobs, keep defaults conservative
  (reject unknown required bits).

