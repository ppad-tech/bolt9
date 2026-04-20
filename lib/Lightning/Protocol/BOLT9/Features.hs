{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT9.Features
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Known feature table for the Lightning Network, per
-- [BOLT #9](https://github.com/lightning/bolts/blob/master/09-features.md).

module Lightning.Protocol.BOLT9.Features (
    -- * Feature
    Feature(..)

    -- * Known feature
  , KnownFeature(..)
  , knownFeatureByBit
  , knownFeatureByName

    -- * Lookup
  , featureByBit
  , featureByName

    -- * Known features table
  , knownFeatures
  ) where

import Control.DeepSeq (NFData)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Word (Word16)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT9.Types (Context(..))

-- | A known feature from the BOLT #9 specification.
data Feature = Feature {
    featureName         :: !String
    -- ^ The canonical name of the feature.
  , featureBaseBit      :: {-# UNPACK #-} !Word16
    -- ^ The even (compulsory) bit number; the odd (optional) bit is
    --   @baseBit + 1@.
  , featureContexts     :: ![Context]
    -- ^ Contexts in which this feature may be presented.
  , featureDependencies :: ![String]
    -- ^ Names of features this feature depends on.
  , featureAssumed      :: !Bool
    -- ^ Whether this feature is assumed to be universally supported.
  }
  deriving (Eq, Show, Generic)

instance NFData Feature

-- | A feature that is known to be in the BOLT #9 specification.
--
-- Constructed via 'knownFeatureByBit' or 'knownFeatureByName',
-- which validate that the feature exists in the known table.
newtype KnownFeature = KnownFeature {
    unKnownFeature :: Feature
    -- ^ Extract the underlying 'Feature'.
  }
  deriving (Eq, Show, Generic)

instance NFData KnownFeature

-- | Look up a known feature by bit number.
--
--   Accepts either the even (compulsory) or odd (optional) bit.
--
--   >>> fmap (featureName . unKnownFeature) (knownFeatureByBit 16)
--   Just "basic_mpp"
--   >>> knownFeatureByBit 999
--   Nothing
knownFeatureByBit :: Word16 -> Maybe KnownFeature
knownFeatureByBit !bit = fmap KnownFeature (featureByBit bit)
{-# INLINE knownFeatureByBit #-}

-- | Look up a known feature by its canonical name.
--
--   >>> fmap (featureName . unKnownFeature)
--   ...       (knownFeatureByName "basic_mpp")
--   Just "basic_mpp"
--   >>> knownFeatureByName "nonexistent"
--   Nothing
knownFeatureByName :: String -> Maybe KnownFeature
knownFeatureByName !name =
  fmap KnownFeature (featureByName name)
{-# INLINE knownFeatureByName #-}

-- | The complete table of known features from BOLT #9.
knownFeatures :: [Feature]
knownFeatures = [
    Feature "option_data_loss_protect" 0 [] [] True
  , Feature "option_upfront_shutdown_script" 4 [Init, NodeAnn] [] False
  , Feature "gossip_queries" 6 [] [] False
  , Feature "var_onion_optin" 8 [] [] True
  , Feature "gossip_queries_ex" 10 [Init, NodeAnn] [] False
  , Feature "option_static_remotekey" 12 [] [] True
  , Feature "payment_secret" 14 [] [] True
  , Feature "basic_mpp" 16 [Init, NodeAnn, Invoice]
      ["payment_secret"] False
  , Feature "option_support_large_channel" 18 [Init, NodeAnn] [] False
  , Feature "option_anchors" 22 [Init, NodeAnn, ChanType] [] False
  , Feature "option_route_blinding" 24 [Init, NodeAnn, Invoice] [] False
  , Feature "option_shutdown_anysegwit" 26 [Init, NodeAnn] [] False
  , Feature "option_dual_fund" 28 [Init, NodeAnn] [] False
  , Feature "option_quiesce" 34 [Init, NodeAnn] [] False
  , Feature "option_attribution_data" 36 [Init, NodeAnn, Invoice]
      [] False
  , Feature "option_onion_messages" 38 [Init, NodeAnn] [] False
  , Feature "option_provide_storage" 42 [Init, NodeAnn] [] False
  , Feature "option_channel_type" 44 [] [] True
  , Feature "option_scid_alias" 46 [Init, NodeAnn, ChanType] [] False
  , Feature "option_payment_metadata" 48 [Invoice] [] False
  , Feature "option_zeroconf" 50 [Init, NodeAnn, ChanType]
      ["option_scid_alias"] False
  , Feature "option_simple_close" 60 [Init, NodeAnn]
      ["option_shutdown_anysegwit"] False
  ]

-- | Look up a feature by bit number.
--
--   Accepts either the even (compulsory) or odd (optional) bit of the pair.
--
--   >>> fmap featureName (featureByBit 16)
--   Just "basic_mpp"
--   >>> fmap featureName (featureByBit 17)  -- odd bit also works
--   Just "basic_mpp"
--   >>> featureByBit 999
--   Nothing
featureByBit :: Word16 -> Maybe Feature
featureByBit !bit =
  let !baseBit = fromIntegral bit - (fromIntegral bit `mod` 2)
  in  IM.lookup baseBit featuresByBit
{-# INLINE featureByBit #-}

-- | Look up a feature by its canonical name.
--
--   >>> fmap featureBaseBit (featureByName "basic_mpp")
--   Just 16
--   >>> featureByName "nonexistent"
--   Nothing
featureByName :: String -> Maybe Feature
featureByName !name = M.lookup name featuresByName
{-# INLINE featureByName #-}

-- Lookup tables -------------------------------------------------------------

-- | Features indexed by base bit (even bit number).
featuresByBit :: IntMap Feature
featuresByBit = IM.fromList
  [(fromIntegral (featureBaseBit f), f) | f <- knownFeatures]

-- | Features indexed by canonical name.
featuresByName :: Map String Feature
featuresByName = M.fromList
  [(featureName f, f) | f <- knownFeatures]
