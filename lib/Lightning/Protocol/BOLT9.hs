{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT9
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Feature flags for the Lightning Network, per
-- [BOLT #9](https://github.com/lightning/bolts/blob/master/09-features.md).
--
-- == Overview
--
-- BOLT #9 defines feature flags that Lightning nodes advertise to indicate
-- support for optional protocol features. Features are represented as bit
-- positions in a variable-length bit vector, where even bits indicate
-- required (compulsory) support and odd bits indicate optional support.
--
-- This library provides:
--
-- * Type-safe feature vectors with efficient bit manipulation
-- * A complete table of known features from the BOLT #9 specification
-- * Validation for both locally-created and remotely-received vectors
-- * Context-aware validation (init, node_announcement, invoice, etc.)
--
-- == Quick Start
--
-- Create a feature vector and set some features:
--
-- >>> import Lightning.Protocol.BOLT9
-- >>> let Just mpp = featureByName "basic_mpp"
-- >>> let fv = setFeature mpp False empty  -- optional support
-- >>> hasFeature mpp fv
-- Just False
--
-- Validate a feature vector for a specific context:
--
-- >>> validateLocal Init fv
-- Left [MissingDependency "basic_mpp" "payment_secret"]
--
-- Fix by adding the dependency:
--
-- >>> let Just ps = featureByName "payment_secret"
-- >>> let fv' = setFeature ps False (setFeature mpp False empty)
-- >>> validateLocal Init fv'
-- Right ()
--
-- == Bit Numbering
--
-- Features use paired bits: even bits (0, 2, 4, ...) indicate required
-- support, while odd bits (1, 3, 5, ...) indicate optional support.
-- For example, @basic_mpp@ uses bit 16 (required) and 17 (optional).
--
-- A node setting bit 16 requires all peers to support @basic_mpp@.
-- A node setting bit 17 indicates optional support (peers without it
-- may still connect).

module Lightning.Protocol.BOLT9 (
    -- * Context
    -- | Contexts specify where feature flags appear in the protocol.
    Context(..)
  , isChannelContext
  , channelParity

    -- * Bit indices
    -- | Low-level bit index types for direct bit manipulation.
  , BitIndex
  , unBitIndex
  , bitIndex

    -- * Required/optional bits
    -- | Type-safe wrappers ensuring correct parity.
  , RequiredBit
  , unRequiredBit
  , requiredBit
  , requiredFromBitIndex

  , OptionalBit
  , unOptionalBit
  , optionalBit
  , optionalFromBitIndex

    -- * Feature vectors
    -- | The core feature vector type and basic operations.
  , FeatureVector
  , unFeatureVector
  , FV.empty
  , fromByteString
  , set
  , clear
  , member

    -- * Known features
    -- | The BOLT #9 feature table and lookup functions.
  , Feature(..)
  , featureByBit
  , featureByName
  , knownFeatures

    -- * Parsing and rendering
    -- | Wire format conversion.
  , parse
  , render

    -- * Low-level bit operations
    -- | Direct bit manipulation by index.
  , Codec.setBit
  , Codec.clearBit
  , Codec.testBit

    -- * Feature operations
    -- | High-level operations using 'Feature' values.
  , setFeature
  , hasFeature
  , listFeatures

    -- * Validation
    -- | Validate feature vectors for correctness.
  , ValidationError(..)
  , validateLocal
  , validateRemote
  , highestSetBit
  , Validate.setBits
  ) where

import Lightning.Protocol.BOLT9.Codec as Codec
import Lightning.Protocol.BOLT9.Features
import Lightning.Protocol.BOLT9.Types as FV
import Lightning.Protocol.BOLT9.Validate as Validate
