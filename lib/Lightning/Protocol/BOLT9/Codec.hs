{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Lightning.Protocol.BOLT9.Codec
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Parsing and rendering for BOLT #9 feature vectors.

module Lightning.Protocol.BOLT9.Codec (
    -- * Parsing and rendering
    parse
  , render

    -- * Bit operations
  , setBit
  , clearBit
  , testBit

    -- * Feature operations
  , setFeature
  , hasFeature
  , listFeatures
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word16)
import Lightning.Protocol.BOLT9.Features
import Lightning.Protocol.BOLT9.Types

-- Parsing and rendering ------------------------------------------------------

-- | Parse a ByteString into a FeatureVector.
--
--   Alias for 'fromByteString'.
parse :: ByteString -> FeatureVector
parse = fromByteString
{-# INLINE parse #-}

-- | Render a FeatureVector to a ByteString, trimming leading zero bytes
--   for compact encoding.
render :: FeatureVector -> ByteString
render = BS.dropWhile (== 0) . unFeatureVector
{-# INLINE render #-}

-- Bit operations -------------------------------------------------------------

-- | Set a bit by raw index.
setBit :: Word16 -> FeatureVector -> FeatureVector
setBit !idx = set (bitIndex idx)
{-# INLINE setBit #-}

-- | Clear a bit by raw index.
clearBit :: Word16 -> FeatureVector -> FeatureVector
clearBit !idx = clear (bitIndex idx)
{-# INLINE clearBit #-}

-- | Test if a bit is set.
testBit :: Word16 -> FeatureVector -> Bool
testBit !idx = member (bitIndex idx)
{-# INLINE testBit #-}

-- Feature operations ---------------------------------------------------------

-- | Set or clear a feature's bit.
--
--   If the Bool is True, sets the required (even) bit.
--   If the Bool is False, sets the optional (odd) bit.
setFeature :: Feature -> Bool -> FeatureVector -> FeatureVector
setFeature !f !required = setBit targetBit
  where
    !baseBit   = featureBaseBit f
    !targetBit = if required then baseBit else baseBit + 1
{-# INLINE setFeature #-}

-- | Check if a feature is set in the vector.
--
--   Returns:
--
--   * @Just True@ if the required (even) bit is set
--   * @Just False@ if the optional (odd) bit is set (and required is not)
--   * @Nothing@ if neither bit is set
hasFeature :: Feature -> FeatureVector -> Maybe Bool
hasFeature !f !fv
  | testBit baseBit fv       = Just True   -- required
  | testBit (baseBit + 1) fv = Just False  -- optional
  | otherwise                = Nothing
  where
    !baseBit = featureBaseBit f
{-# INLINE hasFeature #-}

-- | List all known features that are set in the vector.
--
--   Returns pairs of (Feature, Bool) where the Bool indicates if the
--   required (even) bit is set (True) or the optional (odd) bit (False).
listFeatures :: FeatureVector -> [(Feature, Bool)]
listFeatures !fv = foldr check [] knownFeatures
  where
    check !f !acc = case hasFeature f fv of
      Just isReq -> (f, isReq) : acc
      Nothing    -> acc
