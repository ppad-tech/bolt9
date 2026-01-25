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

module Lightning.Protocol.BOLT9 (
    -- * Context
    Context(..)
  , isChannelContext
  , channelParity

    -- * Bit indices
  , BitIndex
  , unBitIndex
  , bitIndex

    -- * Required/optional bits
  , RequiredBit
  , unRequiredBit
  , requiredBit
  , requiredFromBitIndex

  , OptionalBit
  , unOptionalBit
  , optionalBit
  , optionalFromBitIndex

    -- * Feature vectors
  , FeatureVector
  , unFeatureVector
  , FV.empty
  , fromByteString
  , set
  , clear
  , member

    -- * Known features
  , Feature(..)
  , featureByBit
  , featureByName
  , knownFeatures

    -- * Parsing and rendering
  , parse
  , render

    -- * Codec bit operations
  , Codec.setBit
  , Codec.clearBit
  , Codec.testBit

    -- * Codec feature operations
  , setFeature
  , hasFeature
  , listFeatures

    -- * Validation
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
