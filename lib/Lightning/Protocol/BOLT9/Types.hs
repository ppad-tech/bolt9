{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT9.Types
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Baseline types for BOLT #9 feature flags.

module Lightning.Protocol.BOLT9.Types (
    -- * Context
    Context(..)
  , isChannelContext
  , channelParity

    -- * Bit indices
  , BitIndex
  , unBitIndex
  , bitIndex

    -- * Required/optional level
  , FeatureLevel(..)

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
  , empty
  , fromByteString
  , set
  , clear
  , member
  ) where

import Control.DeepSeq (NFData)
import qualified Data.Bits as B
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16)
import GHC.Generics (Generic)

-- Context ------------------------------------------------------------------

-- | Presentation context for feature flags.
--
-- Per BOLT #9, features are presented in different message contexts:
--
-- * 'Init' - the @init@ message
-- * 'NodeAnn' - @node_announcement@ messages
-- * 'ChanAnn' - @channel_announcement@ messages (normal)
-- * 'ChanAnnOdd' - @channel_announcement@, always odd (optional)
-- * 'ChanAnnEven' - @channel_announcement@, always even (required)
-- * 'Invoice' - BOLT 11 invoices
-- * 'Blinded' - @allowed_features@ field of a blinded path
-- * 'ChanType' - @channel_type@ field when opening channels
data Context
  = Init        -- ^ I: presented in the @init@ message
  | NodeAnn     -- ^ N: presented in @node_announcement@ messages
  | ChanAnn     -- ^ C: presented in @channel_announcement@ message
  | ChanAnnOdd  -- ^ C-: @channel_announcement@, always odd (optional)
  | ChanAnnEven -- ^ C+: @channel_announcement@, always even (required)
  | Invoice     -- ^ 9: presented in BOLT 11 invoices
  | Blinded     -- ^ B: @allowed_features@ field of a blinded path
  | ChanType    -- ^ T: @channel_type@ field when opening channels
  deriving (Eq, Ord, Show, Generic)

instance NFData Context

-- | Check if a context is a channel announcement context (C, C-, or C+).
isChannelContext :: Context -> Bool
isChannelContext ChanAnn     = True
isChannelContext ChanAnnOdd  = True
isChannelContext ChanAnnEven = True
isChannelContext _           = False
{-# INLINE isChannelContext #-}

-- | For channel contexts with forced parity, return 'Just' the required
-- parity: 'True' for even (C+), 'False' for odd (C-). Returns 'Nothing'
-- for contexts without forced parity.
channelParity :: Context -> Maybe Bool
channelParity ChanAnnOdd  = Just False  -- odd
channelParity ChanAnnEven = Just True   -- even
channelParity _           = Nothing
{-# INLINE channelParity #-}

-- FeatureLevel -------------------------------------------------------------

-- | Whether a feature is set as required or optional.
--
-- Per BOLT #9, each feature has a pair of bits: the even bit indicates
-- required (compulsory) support, the odd bit indicates optional support.
data FeatureLevel
  = Required  -- ^ The feature is required (even bit set)
  | Optional  -- ^ The feature is optional (odd bit set)
  deriving (Eq, Ord, Show, Generic)

instance NFData FeatureLevel

-- BitIndex -----------------------------------------------------------------

-- | A bit index into a feature vector. Bit 0 is the least significant bit.
--
-- Valid range: 0-65535 (sufficient for any practical feature flag).
newtype BitIndex = BitIndex { unBitIndex :: Word16 }
  deriving (Eq, Ord, Show, Generic)

instance NFData BitIndex

-- | Smart constructor for 'BitIndex'. Always succeeds since all Word16
-- values are valid.
bitIndex :: Word16 -> BitIndex
bitIndex = BitIndex
{-# INLINE bitIndex #-}

-- RequiredBit --------------------------------------------------------------

-- | A required (compulsory) feature bit. Required bits are always even.
newtype RequiredBit = RequiredBit { unRequiredBit :: Word16 }
  deriving (Eq, Ord, Show, Generic)

instance NFData RequiredBit

-- | Smart constructor for 'RequiredBit'. Returns 'Nothing' if the bit
--   index is odd.
--
--   >>> requiredBit 16
--   Just (RequiredBit {unRequiredBit = 16})
--   >>> requiredBit 17
--   Nothing
requiredBit :: Word16 -> Maybe RequiredBit
requiredBit !w
  | w B..&. 1 == 0 = Just (RequiredBit w)
  | otherwise      = Nothing
{-# INLINE requiredBit #-}

-- | Convert a 'BitIndex' to a 'RequiredBit'. Returns 'Nothing' if odd.
requiredFromBitIndex :: BitIndex -> Maybe RequiredBit
requiredFromBitIndex (BitIndex w) = requiredBit w
{-# INLINE requiredFromBitIndex #-}

-- OptionalBit --------------------------------------------------------------

-- | An optional feature bit. Optional bits are always odd.
newtype OptionalBit = OptionalBit { unOptionalBit :: Word16 }
  deriving (Eq, Ord, Show, Generic)

instance NFData OptionalBit

-- | Smart constructor for 'OptionalBit'. Returns 'Nothing' if the bit
--   index is even.
--
--   >>> optionalBit 17
--   Just (OptionalBit {unOptionalBit = 17})
--   >>> optionalBit 16
--   Nothing
optionalBit :: Word16 -> Maybe OptionalBit
optionalBit !w
  | w B..&. 1 == 1 = Just (OptionalBit w)
  | otherwise      = Nothing
{-# INLINE optionalBit #-}

-- | Convert a 'BitIndex' to an 'OptionalBit'. Returns 'Nothing' if even.
optionalFromBitIndex :: BitIndex -> Maybe OptionalBit
optionalFromBitIndex (BitIndex w) = optionalBit w
{-# INLINE optionalFromBitIndex #-}

-- FeatureVector ------------------------------------------------------------

-- | A feature vector represented as a strict ByteString.
--
-- The vector is stored in big-endian byte order (most significant byte
-- first), with bits numbered from the least significant bit of the last
-- byte. Bit 0 is at position 0 of the last byte.
newtype FeatureVector = FeatureVector { unFeatureVector :: ByteString }
  deriving (Eq, Ord, Show, Generic)

instance NFData FeatureVector

-- | The empty feature vector (no features set).
--
--   >>> empty
--   FeatureVector {unFeatureVector = ""}
empty :: FeatureVector
empty = FeatureVector BS.empty
{-# INLINE empty #-}

-- | Wrap a ByteString as a FeatureVector.
fromByteString :: ByteString -> FeatureVector
fromByteString = FeatureVector
{-# INLINE fromByteString #-}

-- | Set a bit in the feature vector.
--
--   >>> set (bitIndex 0) empty
--   FeatureVector {unFeatureVector = "\SOH"}
--   >>> set (bitIndex 8) empty
--   FeatureVector {unFeatureVector = "\SOH\NUL"}
set :: BitIndex -> FeatureVector -> FeatureVector
set (BitIndex idx) (FeatureVector bs) =
  let byteIdx    = fromIntegral idx `div` 8
      bitOffset  = fromIntegral idx `mod` 8
      len        = BS.length bs
      -- Number of bytes needed to hold this bit
      needed     = byteIdx + 1
      -- Pad with zeros if necessary (prepend to maintain big-endian)
      bs'        = if needed > len
                   then BS.replicate (needed - len) 0 <> bs
                   else bs
      len'       = BS.length bs'
      -- Index from the end (big-endian: last byte has lowest bits)
      realIdx    = len' - 1 - byteIdx
      oldByte    = BS.index bs' realIdx
      newByte    = oldByte B..|. B.shiftL 1 bitOffset
  in  FeatureVector (updateByteAt realIdx newByte bs')
{-# INLINE set #-}

-- | Clear a bit in the feature vector.
clear :: BitIndex -> FeatureVector -> FeatureVector
clear (BitIndex idx) (FeatureVector bs)
  | BS.null bs = FeatureVector bs
  | otherwise  =
      let byteIdx   = fromIntegral idx `div` 8
          bitOffset = fromIntegral idx `mod` 8
          len       = BS.length bs
      in  if byteIdx >= len
          then FeatureVector bs  -- bit not in range, already clear
          else
            let realIdx = len - 1 - byteIdx
                oldByte = BS.index bs realIdx
                newByte = oldByte B..&. B.complement (B.shiftL 1 bitOffset)
            in  FeatureVector (stripLeadingZeros (updateByteAt realIdx newByte bs))
{-# INLINE clear #-}

-- | Test if a bit is set in the feature vector.
--
--   >>> member (bitIndex 0) (set (bitIndex 0) empty)
--   True
--   >>> member (bitIndex 1) (set (bitIndex 0) empty)
--   False
member :: BitIndex -> FeatureVector -> Bool
member (BitIndex idx) (FeatureVector bs)
  | BS.null bs = False
  | otherwise  =
      let byteIdx   = fromIntegral idx `div` 8
          bitOffset = fromIntegral idx `mod` 8
          len       = BS.length bs
      in  if byteIdx >= len
          then False
          else
            let realIdx = len - 1 - byteIdx
                byte    = BS.index bs realIdx
            in  byte B..&. B.shiftL 1 bitOffset /= 0
{-# INLINE member #-}

-- Internal helpers ---------------------------------------------------------

-- | Update a single byte at the given index.
updateByteAt :: Int -> Word8 -> ByteString -> ByteString
updateByteAt !i !w !bs =
  let (before, after) = BS.splitAt i bs
  in  case BS.uncons after of
        Nothing      -> bs  -- shouldn't happen if i is valid
        Just (_, rest) -> before <> BS.singleton w <> rest
{-# INLINE updateByteAt #-}

-- | Remove leading zero bytes from a ByteString.
stripLeadingZeros :: ByteString -> ByteString
stripLeadingZeros = BS.dropWhile (== 0)
{-# INLINE stripLeadingZeros #-}
