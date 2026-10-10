{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT9.Validate
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Validation for BOLT #9 feature vectors.

module Lightning.Protocol.BOLT9.Validate (
    -- * Error types
    ValidationError(..)

    -- * Local validation
  , validateLocal

    -- * Remote validation
  , validateRemote

    -- * Validated construction
  , setFeatureForContext
  , validateNoBothBits

    -- * Helpers
  , highestSetBit
  , setBits
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Bits as B
import Data.Word (Word16)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT9.Codec
  (isFeatureSet, setFeature, testBit)
import Lightning.Protocol.BOLT9.Features
import Lightning.Protocol.BOLT9.Types

-- | Validation errors for feature vectors.
data ValidationError
  = BothBitsSet {-# UNPACK #-} !Word16 !String
    -- ^ Both optional and required bits are set for a feature.
    --   Arguments: base bit index, feature name.
  | MissingDependency !String !String
    -- ^ A feature's dependency is not set.
    --   Arguments: feature name, missing dependency name.
  | ContextNotAllowed !String !Context
    -- ^ A feature is not allowed in the given context.
    --   Arguments: feature name, context.
  | UnknownRequiredBit {-# UNPACK #-} !Int
    -- ^ An unknown required (even) bit is set (remote validation only).
    --   Argument: bit index.
  | InvalidParity {-# UNPACK #-} !Int !Context
    -- ^ A bit has invalid parity for a channel context.
    --   Arguments: bit index, context (ChanAnnOdd or ChanAnnEven).
  deriving (Eq, Show, Generic)

instance NFData ValidationError

-- Local validation -----------------------------------------------------------

-- | Validate a feature vector for local use (vectors we create/send).
--
--   Checks:
--
--   * No feature has both optional and required bits set
--   * All set features are valid for the given context
--   * All dependencies of set features are also set
--   * C- context forces odd bits only, C+ forces even bits only
--
--   >>> import Data.Maybe (fromJust)
--   >>> import Lightning.Protocol.BOLT9.Codec (setFeature)
--   >>> let mpp = fromJust (featureByName "basic_mpp")
--   >>> let ps = fromJust (featureByName "payment_secret")
--   >>> validateLocal Init (setFeature mpp False empty)
--   Left [MissingDependency "basic_mpp" "payment_secret"]
--   >>> validateLocal Init (setFeature mpp False (setFeature ps False empty))
--   Right ()
validateLocal :: Context -> FeatureVector -> Either [ValidationError] ()
validateLocal !ctx !fv =
  let errs = bothBitsErrors fv
          ++ contextErrors ctx fv
          ++ dependencyErrors fv
          ++ parityErrors ctx fv
  in  if null errs
      then Right ()
      else Left errs

-- | Check for features with both bits set.
bothBitsErrors :: FeatureVector -> [ValidationError]
bothBitsErrors !fv = foldr check [] knownFeatures
  where
    check !f !acc =
      let !baseBit = featureBaseBit f
      in  if testBit baseBit fv && testBit (baseBit + 1) fv
          then BothBitsSet baseBit (featureName f) : acc
          else acc

-- | Check for features not allowed in the given context.
contextErrors :: Context -> FeatureVector -> [ValidationError]
contextErrors !ctx !fv = foldr check [] knownFeatures
  where
    check !f !acc =
      let !contexts = featureContexts f
      in  if   isFeatureSet f fv
            && not (null contexts)
            && not (contextAllowed ctx contexts)
          then ContextNotAllowed (featureName f) ctx : acc
          else acc

-- | Check if a context is allowed given a list of allowed contexts.
contextAllowed :: Context -> [Context] -> Bool
contextAllowed !ctx !allowed = ctx `elem` allowed || channelMatch
  where
    channelMatch = isChannelContext ctx && any isChannelContext allowed

-- | Check for missing dependencies.
dependencyErrors :: FeatureVector -> [ValidationError]
dependencyErrors !fv = foldr check [] knownFeatures
  where
    check !f !acc =
      if   isFeatureSet f fv
      then checkDeps f (featureDependencies f) ++ acc
      else acc

    checkDeps !f = foldr (checkOneDep f) []

    checkOneDep !f !depName !acc =
      case featureByName depName of
        Nothing   -> acc  -- unknown dep, skip
        Just !dep ->
          if   isFeatureSet dep fv
          then acc
          else MissingDependency (featureName f) depName : acc

-- | Check for parity errors in C- and C+ contexts.
parityErrors :: Context -> FeatureVector -> [ValidationError]
parityErrors !ctx !fv = case channelParity ctx of
  Nothing       -> []
  Just wantEven -> foldr (checkParity wantEven) [] (setBits fv)
  where
    checkParity !wantEven !bit !acc =
      let isEven = bit `mod` 2 == 0
      in  if isEven /= wantEven
          then InvalidParity bit ctx : acc
          else acc

-- Validated construction -------------------------------------------------------

-- | Set a feature in a vector, validating that the feature is
--   allowed in the given context and has correct parity.
--
--   Checks:
--
--   * The feature's context list includes the given context
--     (or is empty, meaning all contexts are allowed)
--   * For 'ChanAnnOdd', only 'Optional' (odd bit) is allowed
--   * For 'ChanAnnEven', only 'Required' (even bit) is allowed
--
--   >>> import Data.Maybe (fromJust)
--   >>> let pm = fromJust (featureByName "option_payment_metadata")
--   >>> setFeatureForContext Invoice pm Optional empty
--   Right ...
--   >>> setFeatureForContext Init pm Optional empty
--   Left (ContextNotAllowed "option_payment_metadata" Init)
setFeatureForContext
  :: Context
  -> Feature
  -> FeatureLevel
  -> FeatureVector
  -> Either ValidationError FeatureVector
setFeatureForContext !ctx !f !level !fv
  | not (null contexts)
  , not (contextAllowed ctx contexts)
  = Left (ContextNotAllowed (featureName f) ctx)
  | otherwise
  = case channelParity ctx of
      Just True | level == Optional ->
        Left (InvalidParity (fromIntegral targetBit) ctx)
      Just False | level == Required ->
        Left (InvalidParity (fromIntegral targetBit) ctx)
      _ -> Right (setFeature f level fv)
  where
    !contexts  = featureContexts f
    !baseBit   = featureBaseBit f
    !targetBit = case level of
      Required -> baseBit
      Optional -> baseBit + 1

-- | Validate that no feature has both its required and optional
--   bits set simultaneously.
--
--   Returns the input vector unchanged on success.
--
--   >>> validateNoBothBits empty
--   Right ...
validateNoBothBits
  :: FeatureVector -> Either ValidationError FeatureVector
validateNoBothBits !fv = go knownFeatures
  where
    go [] = Right fv
    go (f:fs) =
      let !baseBit = featureBaseBit f
      in  if   testBit baseBit fv
            && testBit (baseBit + 1) fv
          then Left (BothBitsSet baseBit (featureName f))
          else go fs

-- Remote validation ----------------------------------------------------------

-- | Validate a feature vector received from a remote peer.
--
--   Checks:
--
--   * Unknown odd (optional) bits are acceptable (ignored)
--   * Unknown even (required) bits are errors
--   * If both bits of a pair are set, treat as required (not an error)
--   * Context restrictions still apply for known features
--   * All dependencies of set features are also set
--
--   >>> import Lightning.Protocol.BOLT9.Codec (setBit)
--   >>> validateRemote Init (setBit 999 empty)  -- unknown odd bit: ok
--   Right ()
--   >>> validateRemote Init (setBit 998 empty)  -- unknown even bit: error
--   Left [UnknownRequiredBit 998]
validateRemote :: Context -> FeatureVector -> Either [ValidationError] ()
validateRemote !ctx !fv =
  let errs = unknownRequiredErrors fv
          ++ contextErrors ctx fv
          ++ dependencyErrors fv
          ++ parityErrors ctx fv
  in  if null errs
      then Right ()
      else Left errs

-- | Check for unknown required bits.
unknownRequiredErrors :: FeatureVector -> [ValidationError]
unknownRequiredErrors !fv = foldr check [] (setBits fv)
  where
    check !bit !acc
      | bit `mod` 2 == 1 = acc  -- odd bit, optional, ignore
      | bit > fromIntegral (maxBound :: Word16) =
          UnknownRequiredBit bit : acc
      | otherwise = case featureByBit (fromIntegral bit) of
          Just _  -> acc  -- known feature
          Nothing -> UnknownRequiredBit bit : acc

-- Helpers --------------------------------------------------------------------

-- | Find the highest set bit in a feature vector.
--
--   Returns 'Nothing' if the vector is empty or has no bits set.
highestSetBit :: FeatureVector -> Maybe Int
highestSetBit !fv =
  let !bs = unFeatureVector fv
  in  if BS.null bs
      then Nothing
      else findHighestBit bs

-- | Find the highest set bit in a non-empty ByteString.
findHighestBit :: ByteString -> Maybe Int
findHighestBit !bs = go 0
  where
    !len = BS.length bs

    go !i
      | i >= len  = Nothing
      | otherwise =
          let !byte = BS.index bs i
          in  if byte == 0
              then go (i + 1)
              else
                let !bytePos = len - 1 - i
                    !highBit = 7 - B.countLeadingZeros byte
                    !bitIdx  = bytePos * 8 + highBit
                in  Just bitIdx

-- | Collect all set bits in a feature vector.
--
--   Returns a list of bit indices in ascending order.
setBits :: FeatureVector -> [Int]
setBits !fv =
  let !bs  = unFeatureVector fv
      !len = BS.length bs
  in  collectBits bs len 0 []

-- | Collect bits from a ByteString into a list.
collectBits :: ByteString -> Int -> Int -> [Int] -> [Int]
collectBits !bs !len !i !acc
  | i >= len  = acc
  | otherwise =
      let !byte    = BS.index bs (len - 1 - i)
          !baseIdx = i * 8
          !acc'    = collectByteBits byte baseIdx acc
      in  collectBits bs len (i + 1) acc'

-- | Collect set bits from a single byte.
collectByteBits :: B.Bits a => a -> Int -> [Int] -> [Int]
collectByteBits !byte !baseIdx = go 7
  where
    go !bit !acc
      | bit < 0        = acc
      | B.testBit byte bit = go (bit - 1) ((baseIdx + bit) : acc)
      | otherwise          = go (bit - 1) acc
