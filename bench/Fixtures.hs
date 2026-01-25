module Fixtures (
    -- * ByteString fixtures
    typicalBytes
  , emptyBytes
  , largeBytes

    -- * FeatureVector fixtures
  , typicalFV
  , emptyFV
  , validFV
  , unknownBitsFV
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Lightning.Protocol.BOLT9 as B9

-- ByteString fixtures --------------------------------------------------------

-- | A typical 8-byte feature vector with several common features set:
--   basic_mpp (bit 17), option_anchors (bit 23), option_route_blinding (bit 25)
typicalBytes :: ByteString
typicalBytes = BS.pack [0x02, 0x82, 0x00, 0x00]

-- | Empty ByteString for parsing.
emptyBytes :: ByteString
emptyBytes = BS.empty

-- | Large 64-byte feature vector for stress testing.
largeBytes :: ByteString
largeBytes = BS.pack $ replicate 64 0xAA

-- FeatureVector fixtures -----------------------------------------------------

-- | A typical feature vector with common features set (optional bits):
--   basic_mpp (17), option_anchors (23), option_route_blinding (25)
typicalFV :: B9.FeatureVector
typicalFV = B9.parse typicalBytes

-- | Empty feature vector.
emptyFV :: B9.FeatureVector
emptyFV = B9.empty

-- | A valid feature vector for validation benchmarks.
--   Sets payment_secret (required for basic_mpp dependency) and basic_mpp.
validFV :: B9.FeatureVector
validFV = B9.setBit 15   -- payment_secret (optional)
        $ B9.setBit 17   -- basic_mpp (optional)
        $ B9.setBit 23   -- option_anchors (optional)
        $ B9.empty

-- | A feature vector with unknown bits for remote validation.
--   Contains a known feature plus an unknown odd bit (safe to ignore).
unknownBitsFV :: B9.FeatureVector
unknownBitsFV = B9.setBit 15   -- payment_secret (optional, known)
              $ B9.setBit 99   -- unknown odd bit (should be ignored)
              $ B9.empty
