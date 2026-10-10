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
  , setFeatureWithDeps
  , hasFeature
  , isFeatureSet
  , listFeatures
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word16)
import Lightning.Protocol.BOLT9.Features
import Lightning.Protocol.BOLT9.Types
  ( FeatureLevel(..)
  , FeatureVector
  , bitIndex
  , clear
  , fromByteString
  , member
  , set
  , unFeatureVector
  )

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
--
--   >>> setBit 17 empty
--   FeatureVector {unFeatureVector = "\STX"}
setBit :: Word16 -> FeatureVector -> FeatureVector
setBit !idx = set (bitIndex idx)
{-# INLINE setBit #-}

-- | Clear a bit by raw index.
--
--   >>> clearBit 17 (setBit 17 empty)
--   FeatureVector {unFeatureVector = ""}
clearBit :: Word16 -> FeatureVector -> FeatureVector
clearBit !idx = clear (bitIndex idx)
{-# INLINE clearBit #-}

-- | Test if a bit is set.
--
--   >>> testBit 17 (setBit 17 empty)
--   True
--   >>> testBit 16 (setBit 17 empty)
--   False
testBit :: Word16 -> FeatureVector -> Bool
testBit !idx = member (bitIndex idx)
{-# INLINE testBit #-}

-- Feature operations ---------------------------------------------------------

-- | Set a feature's bit at the given level.
--
--   'Required' sets the even bit, 'Optional' sets the odd bit. The
--   other bit of the pair is cleared, so both are never set.
--
--   >>> import Data.Maybe (fromJust)
--   >>> let mpp = fromJust (featureByName "basic_mpp")
--   >>> setFeature mpp Optional empty  -- set optional bit (17)
--   FeatureVector {unFeatureVector = "\STX"}
--   >>> setFeature mpp Required empty  -- set required bit (16)
--   FeatureVector {unFeatureVector = "\SOH"}
setFeature :: Feature -> FeatureLevel -> FeatureVector -> FeatureVector
setFeature !f !level = case level of
    Required -> setBit baseBit . clearBit (baseBit + 1)
    Optional -> setBit (baseBit + 1) . clearBit baseBit
  where
    !baseBit = featureBaseBit f
{-# INLINE setFeature #-}

-- | Set a feature and all its transitive dependencies.
--
--   Dependencies that are not yet set are set at the same level as
--   the feature itself; dependencies already set keep their level.
--   Unknown dependencies (not in the known features table) are
--   silently skipped.
--
--   >>> import Data.Maybe (fromJust)
--   >>> let mpp = fromJust (featureByName "basic_mpp")
--   >>> let fv = setFeatureWithDeps mpp Optional empty
--   >>> isFeatureSet mpp fv
--   True
--   >>> let Just ps = featureByName "payment_secret"
--   >>> isFeatureSet ps fv  -- dependency auto-set
--   True
setFeatureWithDeps
  :: Feature -> FeatureLevel -> FeatureVector -> FeatureVector
setFeatureWithDeps !f !level !fv =
  let !fv' = setFeature f level fv
  in  foldr setDep fv' (featureDependencies f)
  where
    setDep !depName !acc = case featureByName depName of
      Nothing  -> acc
      Just dep
        | isFeatureSet dep acc ->
            foldr setDep acc (featureDependencies dep)
        | otherwise ->
            setFeatureWithDeps dep level acc

-- | Check if a feature is set in the vector.
--
--   Returns:
--
--   * @Just Required@ if the required (even) bit is set
--   * @Just Optional@ if the optional (odd) bit is set (and required is not)
--   * @Nothing@ if neither bit is set
--
--   >>> import Data.Maybe (fromJust)
--   >>> let mpp = fromJust (featureByName "basic_mpp")
--   >>> hasFeature mpp (setFeature mpp Optional empty)
--   Just Optional
--   >>> hasFeature mpp (setFeature mpp Required empty)
--   Just Required
--   >>> hasFeature mpp empty
--   Nothing
hasFeature :: Feature -> FeatureVector -> Maybe FeatureLevel
hasFeature !f !fv
  | testBit baseBit fv       = Just Required
  | testBit (baseBit + 1) fv = Just Optional
  | otherwise                = Nothing
  where
    !baseBit = featureBaseBit f
{-# INLINE hasFeature #-}

-- | Check if either bit of a feature is set in the vector.
--
--   >>> import Data.Maybe (fromJust)
--   >>> let mpp = fromJust (featureByName "basic_mpp")
--   >>> isFeatureSet mpp (setFeature mpp Optional empty)
--   True
--   >>> isFeatureSet mpp empty
--   False
isFeatureSet :: Feature -> FeatureVector -> Bool
isFeatureSet !f !fv =
  let !baseBit = featureBaseBit f
  in  testBit baseBit fv || testBit (baseBit + 1) fv
{-# INLINE isFeatureSet #-}

-- | List all known features that are set in the vector.
--
--   Returns pairs of (Feature, FeatureLevel) indicating whether each
--   feature is set as required or optional.
--
--   >>> import Data.Maybe (fromJust)
--   >>> let mpp = fromJust (featureByName "basic_mpp")
--   >>> let ps = fromJust (featureByName "payment_secret")
--   >>> let fv = setFeature mpp Optional (setFeature ps Required empty)
--   >>> map (\(f, l) -> (featureName f, l)) (listFeatures fv)
--   [("payment_secret",Required),("basic_mpp",Optional)]
listFeatures :: FeatureVector -> [(Feature, FeatureLevel)]
listFeatures !fv = foldr check [] knownFeatures
  where
    check !f !acc = case hasFeature f fv of
      Just level -> (f, level) : acc
      Nothing    -> acc
