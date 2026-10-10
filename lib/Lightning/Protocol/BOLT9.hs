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
-- A feature vector is a big-endian bit field. Features are assigned
-- pairs of bits: setting the even bit means the feature is required,
-- setting the odd bit means it is optional (/it's ok to be odd/).
--
-- The examples below assume:
--
-- >>> :set -XOverloadedStrings
-- >>> import Lightning.Protocol.BOLT9
--
-- Build a vector, check it before sending it, and render it:
--
-- >>> let fv = set_feature_with_deps OptionZeroconf Optional empty
-- >>> list_features fv
-- [(OptionScidAlias,Optional),(OptionZeroconf,Optional)]
-- >>> validate_local InitContext fv
-- Right ()
-- >>> render fv
-- "\b\128\NUL\NUL\NUL\NUL\NUL"
--
-- Check a vector received from a peer:
--
-- >>> validate_remote InitContext (parse "\DLE\NUL\NUL")
-- Left (UnknownBit 20 :| [])
-- >>> validate_remote InitContext (parse "\STX\NUL\NUL")
-- Right ()

module Lightning.Protocol.BOLT9 (
  -- * Feature vectors
    FeatureVector
  , parse
  , render
  , empty
  , union

  -- * Bits
  , set_bit
  , clear_bit
  , test_bit
  , set_bits

  -- * Known features
  , Feature(..)
  , known_features
  , feature_by_bit
  , feature_bit
  , feature_name
  , feature_contexts
  , feature_dependencies
  , feature_assumed

  -- * Feature operations
  , FeatureLevel(..)
  , set_feature
  , set_feature_with_deps
  , clear_feature
  , test_feature
  , list_features

  -- * Validation
  , Context(..)
  , ValidationError(..)
  , validate_local
  , validate_remote

  -- * Channel types
  , ChannelType(..)
  , BasicChannelType(..)
  , channel_type
  , channel_type_features
  ) where

import Control.DeepSeq (NFData(..))
import qualified Data.Bits as B
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isNothing)
import Data.Word (Word8)
import GHC.Generics (Generic)

-- feature vectors -----------------------------------------------------------

-- | A feature vector.
--
--   Bit 0 is the least significant bit of the last byte. A vector
--   produced by 'parse' keeps its bytes exactly, so 'render' gives
--   them back unchanged (signatures over gossip messages cover them).
--   Every other operation produces a minimally-encoded vector, i.e.
--   one without leading zero bytes.
--
--   Equality ignores leading zero bytes.
--
--   >>> parse "\NUL\STX" == parse "\STX"
--   True
--   >>> set_bit 1 empty
--   parse "\STX"
newtype FeatureVector = FeatureVector ByteString

instance Eq FeatureVector where
  FeatureVector a == FeatureVector b = strip a == strip b

instance Show FeatureVector where
  showsPrec d (FeatureVector bs) = showParen (d > 10) $
    showString "parse " . showsPrec 11 bs

instance NFData FeatureVector where
  rnf (FeatureVector bs) = rnf bs

-- | Parse a feature vector from its wire bytes. Every byte string is a
--   valid feature vector.
--
--   >>> test_bit 9 (parse "\STX\NUL")
--   True
parse :: ByteString -> FeatureVector
parse = FeatureVector
{-# INLINE parse #-}

-- | The wire bytes of a feature vector: the original bytes of a vector
--   produced by 'parse', and the minimal encoding of any other.
--
--   >>> render (parse "\NUL\STX")
--   "\NUL\STX"
--   >>> render (union (parse "\NUL\STX") empty)
--   "\STX"
render :: FeatureVector -> ByteString
render (FeatureVector bs) = bs
{-# INLINE render #-}

-- | The empty feature vector.
--
--   >>> render empty
--   ""
empty :: FeatureVector
empty = FeatureVector BS.empty

-- | The bitwise OR of two feature vectors.
--
--   BOLT #1 requires the receiver of an @init@ message to combine its
--   @globalfeatures@ and @features@ fields this way.
--
--   >>> set_bits (union (parse "\STX") (parse "\SOH\NUL"))
--   [1,8]
union :: FeatureVector -> FeatureVector -> FeatureVector
union (FeatureVector a) (FeatureVector b) =
  FeatureVector (combine (B..|.) a b)

-- bits ----------------------------------------------------------------------

-- | Set the bit at the given index. Negative indices are ignored.
--
--   >>> render (set_bit 9 empty)
--   "\STX\NUL"
set_bit :: Int -> FeatureVector -> FeatureVector
set_bit !i (FeatureVector bs)
  | i < 0     = FeatureVector (strip bs)
  | otherwise = FeatureVector (combine (B..|.) bs (single i))

-- | Clear the bit at the given index. Negative indices are ignored.
--
--   >>> render (clear_bit 9 (parse "\STX\SOH"))
--   "\SOH"
clear_bit :: Int -> FeatureVector -> FeatureVector
clear_bit !i (FeatureVector bs)
  | i < 0 || i `quot` 8 >= BS.length s = FeatureVector s
  | otherwise = FeatureVector (combine clr s (single i))
  where
    s = strip bs
    clr x y = x B..&. B.complement y

-- | Test the bit at the given index. Negative indices are never set.
--
--   >>> test_bit 9 (parse "\STX\NUL")
--   True
--   >>> test_bit 8 (parse "\STX\NUL")
--   False
test_bit :: Int -> FeatureVector -> Bool
test_bit !i (FeatureVector bs)
  | i < 0 || q >= len = False
  | otherwise         = B.testBit (BS.index bs (len - 1 - q)) r
  where
    len    = BS.length bs
    (q, r) = i `quotRem` 8

-- | The indices of all set bits, in ascending order.
--
--   >>> set_bits (parse "\STX\NUL\SOH")
--   [0,17]
set_bits :: FeatureVector -> [Int]
set_bits (FeatureVector bs) = go 0
  where
    !len = BS.length bs
    go !q
      | q >= len  = []
      | otherwise =
          let !w = BS.index bs (len - 1 - q)
          in  [q * 8 + r | r <- [0 .. 7], B.testBit w r] ++ go (q + 1)

-- Drop leading zero bytes.
strip :: ByteString -> ByteString
strip = BS.dropWhile (== 0)
{-# INLINE strip #-}

-- The minimal encoding of a vector with only bit i set (i >= 0).
single :: Int -> ByteString
single i = BS.cons (B.bit r) (BS.replicate q 0)
  where
    (q, r) = i `quotRem` 8

-- Combine two encodings bytewise, aligned at their last bytes (the
-- shorter one padded with leading zeros), and strip the result.
combine
  :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString -> ByteString
combine f a b = strip (fst (BS.unfoldrN n step 0))
  where
    !la = BS.length a
    !lb = BS.length b
    !n  = max la lb
    byte s ls k
      | k < n - ls = 0
      | otherwise  = BS.index s (k - n + ls)
    step !k = Just (f (byte a la k) (byte b lb k), k + 1)

-- known features ------------------------------------------------------------

-- | The features assigned in the BOLT #9 table.
data Feature
  = OptionDataLossProtect           -- ^ 0/1 @option_data_loss_protect@
  | OptionUpfrontShutdownScript     -- ^ 4/5 @option_upfront_shutdown_script@
  | GossipQueries                   -- ^ 6/7 @gossip_queries@
  | VarOnionOptin                   -- ^ 8/9 @var_onion_optin@
  | GossipQueriesEx                 -- ^ 10/11 @gossip_queries_ex@
  | OptionStaticRemotekey           -- ^ 12/13 @option_static_remotekey@
  | PaymentSecret                   -- ^ 14/15 @payment_secret@
  | BasicMpp                        -- ^ 16/17 @basic_mpp@
  | OptionSupportLargeChannel       -- ^ 18/19 @option_support_large_channel@
  | OptionAnchors                   -- ^ 22/23 @option_anchors@
  | OptionRouteBlinding             -- ^ 24/25 @option_route_blinding@
  | OptionShutdownAnysegwit         -- ^ 26/27 @option_shutdown_anysegwit@
  | OptionDualFund                  -- ^ 28/29 @option_dual_fund@
  | OptionQuiesce                   -- ^ 34/35 @option_quiesce@
  | OptionAttributionData           -- ^ 36/37 @option_attribution_data@
  | OptionOnionMessages             -- ^ 38/39 @option_onion_messages@
  | ZeroFeeCommitments              -- ^ 40/41 @zero_fee_commitments@
  | OptionProvideStorage            -- ^ 42/43 @option_provide_storage@
  | OptionChannelType               -- ^ 44/45 @option_channel_type@
  | OptionScidAlias                 -- ^ 46/47 @option_scid_alias@
  | OptionPaymentMetadata           -- ^ 48/49 @option_payment_metadata@
  | OptionZeroconf                  -- ^ 50/51 @option_zeroconf@
  | OptionSimpleClose               -- ^ 60/61 @option_simple_close@
  | OptionSplice                    -- ^ 62/63 @option_splice@
  | OptionOnionMessagesOnlyChannels
    -- ^ 66/67 @option_onion_messages_only_channels@
  deriving (Eq, Ord, Show, Generic)

instance NFData Feature

-- | All known features, in bit order.
--
--   >>> length known_features
--   25
known_features :: [Feature]
known_features = [
    OptionDataLossProtect
  , OptionUpfrontShutdownScript
  , GossipQueries
  , VarOnionOptin
  , GossipQueriesEx
  , OptionStaticRemotekey
  , PaymentSecret
  , BasicMpp
  , OptionSupportLargeChannel
  , OptionAnchors
  , OptionRouteBlinding
  , OptionShutdownAnysegwit
  , OptionDualFund
  , OptionQuiesce
  , OptionAttributionData
  , OptionOnionMessages
  , ZeroFeeCommitments
  , OptionProvideStorage
  , OptionChannelType
  , OptionScidAlias
  , OptionPaymentMetadata
  , OptionZeroconf
  , OptionSimpleClose
  , OptionSplice
  , OptionOnionMessagesOnlyChannels
  ]

-- A row of the BOLT #9 table.
data Entry = Entry {
    e_bit      :: !Int
  , e_name     :: !String
  , e_contexts :: ![Context]
  , e_deps     :: ![Feature]
  , e_assumed  :: !Bool
  }

entry :: Feature -> Entry
entry f = case f of
  OptionDataLossProtect ->
    Entry 0 "option_data_loss_protect" [] [] True
  OptionUpfrontShutdownScript ->
    Entry 4 "option_upfront_shutdown_script" _IN [] False
  GossipQueries ->
    Entry 6 "gossip_queries" [] [] False
  VarOnionOptin ->
    Entry 8 "var_onion_optin" [] [] True
  GossipQueriesEx ->
    Entry 10 "gossip_queries_ex" _IN [] False
  OptionStaticRemotekey ->
    Entry 12 "option_static_remotekey" [] [] True
  PaymentSecret ->
    Entry 14 "payment_secret" [] [] True
  BasicMpp ->
    Entry 16 "basic_mpp" _IN9 [PaymentSecret] False
  OptionSupportLargeChannel ->
    Entry 18 "option_support_large_channel" _IN [] False
  OptionAnchors ->
    Entry 22 "option_anchors" _INT [] False
  OptionRouteBlinding ->
    Entry 24 "option_route_blinding" _IN9 [] False
  OptionShutdownAnysegwit ->
    Entry 26 "option_shutdown_anysegwit" _IN [] False
  OptionDualFund ->
    Entry 28 "option_dual_fund" _IN [] False
  OptionQuiesce ->
    Entry 34 "option_quiesce" _IN [] False
  OptionAttributionData ->
    Entry 36 "option_attribution_data" _IN9 [] False
  OptionOnionMessages ->
    Entry 38 "option_onion_messages" _IN [] False
  ZeroFeeCommitments ->
    Entry 40 "zero_fee_commitments" _IN [OptionChannelType] False
  OptionProvideStorage ->
    Entry 42 "option_provide_storage" _IN [] False
  OptionChannelType ->
    Entry 44 "option_channel_type" [] [] True
  OptionScidAlias ->
    Entry 46 "option_scid_alias" _INT [] False
  OptionPaymentMetadata ->
    Entry 48 "option_payment_metadata" [InvoiceContext] [] False
  OptionZeroconf ->
    Entry 50 "option_zeroconf" _INT [OptionScidAlias] False
  OptionSimpleClose ->
    Entry 60 "option_simple_close" _IN [OptionShutdownAnysegwit] False
  OptionSplice ->
    Entry 62 "option_splice" _IN [] False
  OptionOnionMessagesOnlyChannels ->
    Entry 66 "option_onion_messages_only_channels" _IN
      [OptionOnionMessages] False
  where
    _IN  = [InitContext, NodeContext]
    _IN9 = [InitContext, NodeContext, InvoiceContext]
    _INT = [InitContext, NodeContext, ChannelTypeContext]

-- | The feature that a bit (even or odd) belongs to, if any.
--
--   >>> feature_by_bit 16
--   Just BasicMpp
--   >>> feature_by_bit 17
--   Just BasicMpp
--   >>> feature_by_bit 20
--   Nothing
feature_by_bit :: Int -> Maybe Feature
feature_by_bit !i
  | i < 0 || i > max_known_bit = Nothing
  | otherwise = find ((== base) . feature_bit) known_features
  where
    base = i - i `rem` 2

-- The highest bit assigned to any known feature.
max_known_bit :: Int
max_known_bit = foldr (max . (+ 1) . feature_bit) 0 known_features

-- | A feature's even (required) bit. Its odd (optional) bit is the
--   next one up.
--
--   >>> feature_bit BasicMpp
--   16
feature_bit :: Feature -> Int
feature_bit = e_bit . entry

-- | A feature's name in the BOLT #9 table.
--
--   >>> feature_name BasicMpp
--   "basic_mpp"
feature_name :: Feature -> String
feature_name = e_name . entry

-- | The contexts the BOLT #9 table lists for a feature, in table order.
--   The list is empty for ASSUMED features and for 'GossipQueries'.
--
--   >>> feature_contexts BasicMpp
--   [InitContext,NodeContext,InvoiceContext]
--   >>> feature_contexts PaymentSecret
--   []
feature_contexts :: Feature -> [Context]
feature_contexts = e_contexts . entry

-- | A feature's direct dependencies.
--
--   >>> feature_dependencies OptionZeroconf
--   [OptionScidAlias]
feature_dependencies :: Feature -> [Feature]
feature_dependencies = e_deps . entry

-- | Whether BOLT #9 marks a feature ASSUMED, i.e. supported by all
--   nodes.
--
--   >>> feature_assumed PaymentSecret
--   True
feature_assumed :: Feature -> Bool
feature_assumed = e_assumed . entry

-- feature operations --------------------------------------------------------

-- | The level at which a feature is set.
data FeatureLevel
  = Required  -- ^ the even bit is set
  | Optional  -- ^ the odd bit is set
  deriving (Eq, Ord, Show, Generic)

instance NFData FeatureLevel

level_bit :: Feature -> FeatureLevel -> Int
level_bit f Required = feature_bit f
level_bit f Optional = feature_bit f + 1
{-# INLINE level_bit #-}

-- | Set a feature at the given level, clearing the other bit of its
--   pair.
--
--   >>> set_bits (set_feature BasicMpp Optional empty)
--   [17]
--   >>> set_bits (set_feature BasicMpp Required (set_bit 17 empty))
--   [16]
set_feature :: Feature -> FeatureLevel -> FeatureVector -> FeatureVector
set_feature f l = set_bit (level_bit f l) . clear_bit (level_bit f other)
  where
    other = case l of
      Required -> Optional
      Optional -> Required

-- | Set a feature at the given level, together with its transitive
--   dependencies (ASSUMED ones included). Dependencies that are not
--   yet set are set at the same level; those already set keep theirs.
--
--   >>> list_features (set_feature_with_deps BasicMpp Optional empty)
--   [(PaymentSecret,Optional),(BasicMpp,Optional)]
set_feature_with_deps
  :: Feature -> FeatureLevel -> FeatureVector -> FeatureVector
set_feature_with_deps f l fv =
    foldr ensure (set_feature f l fv) (feature_dependencies f)
  where
    ensure d acc =
      let acc' = case test_feature d acc of
            Just _  -> acc
            Nothing -> set_feature d l acc
      in  foldr ensure acc' (feature_dependencies d)

-- | Clear both bits of a feature.
--
--   >>> clear_feature BasicMpp (set_feature BasicMpp Optional empty)
--   parse ""
clear_feature :: Feature -> FeatureVector -> FeatureVector
clear_feature f =
  clear_bit (level_bit f Required) . clear_bit (level_bit f Optional)

-- | The level at which a feature is set, if it is.
--
--   A feature with both bits set is 'Required', as BOLT #9 directs the
--   receiver to treat it.
--
--   >>> test_feature BasicMpp (set_bit 17 empty)
--   Just Optional
--   >>> test_feature BasicMpp (set_bit 16 (set_bit 17 empty))
--   Just Required
--   >>> test_feature BasicMpp empty
--   Nothing
test_feature :: Feature -> FeatureVector -> Maybe FeatureLevel
test_feature f fv
  | test_bit (level_bit f Required) fv = Just Required
  | test_bit (level_bit f Optional) fv = Just Optional
  | otherwise                          = Nothing

-- | The known features set in a vector, in bit order.
--
--   >>> list_features (parse "\SOH\128\NUL")
--   [(PaymentSecret,Optional),(BasicMpp,Required)]
list_features :: FeatureVector -> [(Feature, FeatureLevel)]
list_features fv =
  [(f, l) | f <- known_features, Just l <- [test_feature f fv]]

-- validation ----------------------------------------------------------------

-- | A field in which feature bits are presented: the values of the
--   Context column of the BOLT #9 table.
--
--   The table's @C-@ and @C+@ are not further contexts. They mark a
--   feature presented in @channel_announcement@ as always optional or
--   always required. No feature at the implemented spec revision is
--   presented in @channel_announcement@ in any form.
data Context
  = InitContext         -- ^ @I@: the @init@ message
  | NodeContext         -- ^ @N@: @node_announcement@
  | ChannelContext      -- ^ @C@: @channel_announcement@
  | InvoiceContext      -- ^ @9@: BOLT #11 invoices
  | BlindedContext      -- ^ @B@: @allowed_features@ of a blinded path
  | ChannelTypeContext  -- ^ @T@: the @channel_type@ field
  deriving (Eq, Ord, Show, Generic)

instance NFData Context

-- | A rule a feature vector breaks.
data ValidationError
  = UnknownBit {-# UNPACK #-} !Int
    -- ^ a bit belonging to no known feature is set
  | BothBitsSet !Feature
    -- ^ both bits of a feature are set
  | ContextNotAllowed !Feature !Context
    -- ^ a feature is set in a context that doesn't present it
  | MissingDependency !Feature !Feature
    -- ^ a set feature (first) lacks a dependency (second)
  | InvalidChannelType
    -- ^ a @channel_type@ is not a type that BOLT #2 defines
  deriving (Eq, Show, Generic)

instance NFData ValidationError

-- | Validate a feature vector that we're about to send in the given
--   context, per the origin requirements of BOLT #9 and BOLT #1.
--
--   For 'ChannelTypeContext', the vector must be a channel type that
--   BOLT #2 defines (see 'channel_type'). For every other context,
--   checks that:
--
--   * no bit outside the known features is set;
--   * no feature has both bits set;
--   * each set feature is presented in the context. A feature with an
--     empty Context column (the ASSUMED features and 'GossipQueries')
--     is accepted in 'InitContext', 'NodeContext' and
--     'InvoiceContext', where deployed nodes still present it;
--   * each set feature's dependencies are set. A dependency on an
--     ASSUMED feature counts as met even when its bits are clear.
--
--   Errors are reported in that order. No known feature is presented
--   in 'ChannelContext' or 'BlindedContext', so only the empty vector
--   passes there (as BOLT #4 requires of @allowed_features@).
--
--   >>> validate_local InitContext (set_feature OptionZeroconf Optional empty)
--   Left (MissingDependency OptionZeroconf OptionScidAlias :| [])
--   >>> validate_local InitContext (set_feature BasicMpp Optional empty)
--   Right ()
--   >>> validate_local InitContext (set_bit 101 empty)
--   Left (UnknownBit 101 :| [])
validate_local
  :: Context -> FeatureVector -> Either (NonEmpty ValidationError) ()
validate_local ctx fv = to_either $ case ctx of
  ChannelTypeContext -> channel_type_errors fv
  _ ->  unknown_bits (const True) fv
     ++ both_bits_errors fv
     ++ context_errors ctx fv
     ++ dependency_errors fv

-- | Validate a feature vector received in the given context, per the
--   receiver requirements for that context:
--
--   * 'InitContext' (BOLT #1) and 'InvoiceContext' (BOLT #11): no
--     unknown even bit is set, and each set feature's dependencies are
--     set (ASSUMED dependencies count as met).
--   * 'NodeContext' and 'ChannelContext' (BOLT #7): no unknown even
--     bit is set. A failure here means the node or channel must not be
--     routed through; the announcement itself is still valid.
--   * 'BlindedContext' (BOLT #4): no unknown bit is set, odd or even.
--   * 'ChannelTypeContext' (BOLT #2): the vector is a defined channel
--     type (see 'channel_type').
--
--   Unknown odd bits are otherwise ignored. A feature with both bits
--   set is not an error; it counts as 'Required' (see 'test_feature').
--   Nor is a known feature outside its listed contexts, since that
--   rule binds only the sender.
--
--   >>> validate_remote InitContext (set_bit 101 empty)
--   Right ()
--   >>> validate_remote InitContext (set_bit 100 empty)
--   Left (UnknownBit 100 :| [])
--   >>> validate_remote BlindedContext (set_bit 101 empty)
--   Left (UnknownBit 101 :| [])
validate_remote
  :: Context -> FeatureVector -> Either (NonEmpty ValidationError) ()
validate_remote ctx fv = to_either $ case ctx of
  InitContext        -> unknown_even ++ dependency_errors fv
  NodeContext        -> unknown_even
  ChannelContext     -> unknown_even
  InvoiceContext     -> unknown_even ++ dependency_errors fv
  BlindedContext     -> unknown_bits (const True) fv
  ChannelTypeContext -> channel_type_errors fv
  where
    unknown_even = unknown_bits even fv

to_either :: [e] -> Either (NonEmpty e) ()
to_either []       = Right ()
to_either (e : es) = Left (e :| es)

-- Unknown set bits satisfying the predicate.
unknown_bits :: (Int -> Bool) -> FeatureVector -> [ValidationError]
unknown_bits p fv =
  [ UnknownBit i
  | i <- set_bits fv, p i, isNothing (feature_by_bit i) ]

both_bits_errors :: FeatureVector -> [ValidationError]
both_bits_errors fv =
  [ BothBitsSet f
  | f <- known_features
  , test_bit (level_bit f Required) fv
  , test_bit (level_bit f Optional) fv ]

context_errors :: Context -> FeatureVector -> [ValidationError]
context_errors ctx fv =
  [ ContextNotAllowed f ctx
  | (f, _) <- list_features fv, not (presented f) ]
  where
    presented f = case feature_contexts f of
      [] -> ctx `elem` [InitContext, NodeContext, InvoiceContext]
      cs -> ctx `elem` cs

dependency_errors :: FeatureVector -> [ValidationError]
dependency_errors fv =
  [ MissingDependency f d
  | (f, _) <- list_features fv
  , d <- feature_dependencies f
  , not (feature_assumed d)
  , isNothing (test_feature d fv) ]

channel_type_errors :: FeatureVector -> [ValidationError]
channel_type_errors fv = case channel_type fv of
  Nothing -> [InvalidChannelType]
  Just _  -> []

-- channel types -------------------------------------------------------------

-- | The basic channel types of BOLT #2.
data BasicChannelType
  = BasicStaticRemotekey
    -- ^ 'OptionStaticRemotekey' (bit 12)
  | BasicAnchors
    -- ^ 'OptionAnchors' and 'OptionStaticRemotekey' (bits 22 and 12)
  | BasicZeroFeeCommitments
    -- ^ 'ZeroFeeCommitments' (bit 40)
  deriving (Eq, Ord, Show, Generic)

instance NFData BasicChannelType

-- | A channel type, per BOLT #2: a basic type plus any of the
--   'OptionScidAlias' and 'OptionZeroconf' variations.
--
--   Channel types are an enumeration that reuses even feature bits;
--   they aren't subject to the dependency or context rules for
--   feature vectors. BOLT #2 also forbids 'OptionScidAlias' in the
--   channel type of a channel to be announced, which needs the
--   @announce_channel@ flag to check.
data ChannelType = ChannelType {
    ct_basic      :: !BasicChannelType
  , ct_scid_alias :: !Bool
  , ct_zeroconf   :: !Bool
  }
  deriving (Eq, Show, Generic)

instance NFData ChannelType

-- | The channel type that a @channel_type@ vector encodes, if it is a
--   defined one. Leading zero bytes are ignored.
--
--   >>> let zeroconf = ChannelType BasicStaticRemotekey False True
--   >>> channel_type (parse "\EOT\NUL\NUL\NUL\NUL\DLE\NUL") == Just zeroconf
--   True
--   >>> channel_type (set_bit 13 empty)
--   Nothing
channel_type :: FeatureVector -> Maybe ChannelType
channel_type fv = find ((== fv) . channel_type_features) channel_types

-- All defined channel types.
channel_types :: [ChannelType]
channel_types =
  [ ChannelType b s z
  | b <- [BasicStaticRemotekey, BasicAnchors, BasicZeroFeeCommitments]
  , s <- [False, True]
  , z <- [False, True] ]

-- | The @channel_type@ vector for a channel type.
--
--   >>> let anchors = ChannelType BasicAnchors False False
--   >>> render (channel_type_features anchors)
--   "@\DLE\NUL"
channel_type_features :: ChannelType -> FeatureVector
channel_type_features (ChannelType b s z) =
    foldr (\f -> set_feature f Required) empty
      (basic b ++ [OptionScidAlias | s] ++ [OptionZeroconf | z])
  where
    basic BasicStaticRemotekey    = [OptionStaticRemotekey]
    basic BasicAnchors            = [OptionAnchors, OptionStaticRemotekey]
    basic BasicZeroFeeCommitments = [ZeroFeeCommitments]
