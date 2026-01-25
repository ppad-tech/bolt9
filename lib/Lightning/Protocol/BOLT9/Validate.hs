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
import Lightning.Protocol.BOLT9.Codec (isFeatureSet, testBit)
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
  | UnknownRequiredBit {-# UNPACK #-} !Word16
    -- ^ An unknown required (even) bit is set (remote validation only).
    --   Argument: bit index.
  | InvalidParity {-# UNPACK #-} !Word16 !Context
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

-- Remote validation ----------------------------------------------------------

-- | Validate a feature vector received from a remote peer.
--
--   Checks:
--
--   * Unknown odd (optional) bits are acceptable (ignored)
--   * Unknown even (required) bits are errors
--   * If both bits of a pair are set, treat as required (not an error)
--   * Context restrictions still apply for known features
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
      | otherwise = case featureByBit bit of
          Just _  -> acc  -- known feature
          Nothing -> UnknownRequiredBit bit : acc

-- Helpers --------------------------------------------------------------------

-- | Find the highest set bit in a feature vector.
--
--   Returns 'Nothing' if the vector is empty or has no bits set.
highestSetBit :: FeatureVector -> Maybe Word16
highestSetBit !fv =
  let !bs = unFeatureVector fv
  in  if BS.null bs
      then Nothing
      else findHighestBit bs

-- | Find the highest set bit in a non-empty ByteString.
findHighestBit :: ByteString -> Maybe Word16
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
                    !bitIdx  = fromIntegral bytePos * 8 + fromIntegral highBit
                in  Just bitIdx

-- | Collect all set bits in a feature vector.
--
--   Returns a list of bit indices in ascending order.
setBits :: FeatureVector -> [Word16]
setBits !fv =
  let !bs  = unFeatureVector fv
      !len = BS.length bs
  in  collectBits bs len 0 []

-- | Collect bits from a ByteString into a list.
collectBits :: ByteString -> Int -> Int -> [Word16] -> [Word16]
collectBits !bs !len !i !acc
  | i >= len  = acc
  | otherwise =
      let !byte    = BS.index bs (len - 1 - i)
          !baseIdx = fromIntegral i * 8
          !acc'    = collectByteBits byte baseIdx acc
      in  collectBits bs len (i + 1) acc'

-- | Collect set bits from a single byte.
collectByteBits :: B.Bits a => a -> Word16 -> [Word16] -> [Word16]
collectByteBits !byte !baseIdx = go 7
  where
    go !bit !acc
      | bit < 0        = acc
      | B.testBit byte bit = go (bit - 1) ((baseIdx + fromIntegral bit) : acc)
      | otherwise          = go (bit - 1) acc
