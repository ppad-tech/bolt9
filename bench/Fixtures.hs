module Fixtures (
    typical
  , padded
  , large
  , maximal
  , zeroconf_type
  , zeroconf_type_features
  ) where

import qualified Data.ByteString as BS
import qualified Lightning.Protocol.BOLT9 as B9

-- | An init feature vector of the sort deployed nodes send (8 bytes).
typical :: B9.FeatureVector
typical = foldr B9.set_bit B9.empty
  [1, 5, 7, 8, 11, 12, 14, 17, 23, 25, 27, 35, 39, 45, 47, 51, 61]

-- | 'typical' with four leading zero bytes.
padded :: B9.FeatureVector
padded = B9.parse (BS.replicate 4 0 <> B9.render typical)

-- | 64 bytes with every odd bit set.
large :: B9.FeatureVector
large = B9.parse (BS.replicate 64 0xAA)

-- | A maximum-length (65535-byte) vector with every bit set.
maximal :: B9.FeatureVector
maximal = B9.parse (BS.replicate 65535 0xFF)

-- | An anchors channel type with both variations.
zeroconf_type :: B9.ChannelType
zeroconf_type = B9.ChannelType B9.BasicAnchors True True

-- | The vector for 'zeroconf_type'.
zeroconf_type_features :: B9.FeatureVector
zeroconf_type_features = B9.channel_type_features zeroconf_type
