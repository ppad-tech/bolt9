{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Bits as B
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (elemIndex, subsequences)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Word (Word8)
import Lightning.Protocol.BOLT9
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "ppad-bolt9" [
    table_tests
  , vector_tests
  , feature_tests
  , local_tests
  , remote_tests
  , invoice_tests
  , channel_type_tests
  , properties
  ]

-- feature table -------------------------------------------------------------

-- | The BOLT #9 table at lightning/bolts@1aadb719: bits, name, whether
--   it is ASSUMED, the Context column, and the dependency names.
spec_table :: [(Int, String, Bool, String, [String])]
spec_table = [
    (0,  "option_data_loss_protect", True, "", [])
  , (4,  "option_upfront_shutdown_script", False, "IN", [])
  , (6,  "gossip_queries", False, "", [])
  , (8,  "var_onion_optin", True, "", [])
  , (10, "gossip_queries_ex", False, "IN", [])
  , (12, "option_static_remotekey", True, "", [])
  , (14, "payment_secret", True, "", [])
  , (16, "basic_mpp", False, "IN9", ["payment_secret"])
  , (18, "option_support_large_channel", False, "IN", [])
  , (22, "option_anchors", False, "INT", [])
  , (24, "option_route_blinding", False, "IN9", [])
  , (26, "option_shutdown_anysegwit", False, "IN", [])
  , (28, "option_dual_fund", False, "IN", [])
  , (34, "option_quiesce", False, "IN", [])
  , (36, "option_attribution_data", False, "IN9", [])
  , (38, "option_onion_messages", False, "IN", [])
  , (40, "zero_fee_commitments", False, "IN", ["option_channel_type"])
  , (42, "option_provide_storage", False, "IN", [])
  , (44, "option_channel_type", True, "", [])
  , (46, "option_scid_alias", False, "INT", [])
  , (48, "option_payment_metadata", False, "9", [])
  , (50, "option_zeroconf", False, "INT", ["option_scid_alias"])
  , (60, "option_simple_close", False, "IN"
    , ["option_shutdown_anysegwit"])
  , (62, "option_splice", False, "IN", [])
  , (66, "option_onion_messages_only_channels", False, "IN"
    , ["option_onion_messages"])
  ]

table_row :: Feature -> (Int, String, Bool, String, [String])
table_row f =
  ( feature_bit f
  , feature_name f
  , feature_assumed f
  , concatMap letter (feature_contexts f)
  , fmap feature_name (feature_dependencies f) )
  where
    letter c = case c of
      InitContext        -> "I"
      NodeContext        -> "N"
      ChannelContext     -> "C"
      InvoiceContext     -> "9"
      BlindedContext     -> "B"
      ChannelTypeContext -> "T"

table_tests :: TestTree
table_tests = testGroup "feature table" [
    testCase "matches the spec table" $
      fmap table_row known_features @?= spec_table

  , testCase "feature_by_bit maps both bits to the feature" $
      mapM_ (\f -> do
               feature_by_bit (feature_bit f) @?= Just f
               feature_by_bit (feature_bit f + 1) @?= Just f)
            known_features

  , testCase "feature_by_bit rejects unassigned bits" $ do
      let assigned = concatMap (\f -> [feature_bit f, feature_bit f + 1])
                               known_features
          free = filter (`notElem` assigned) [0 .. 600]
      mapM_ (\i -> feature_by_bit i @?= Nothing) free
      feature_by_bit (-1) @?= Nothing
      feature_by_bit (-2) @?= Nothing
  ]

-- feature vectors -----------------------------------------------------------

-- | A vector of (n + 1) bytes whose most significant byte is b.
long_vector :: Int -> Word8 -> FeatureVector
long_vector n b = parse (BS.cons b (BS.replicate n 0))

vector_tests :: TestTree
vector_tests = testGroup "feature vectors" [
    testCase "empty" $ do
      render empty @?= ""
      set_bits empty @?= []

  , testCase "render returns parsed bytes exactly" $ do
      render (parse "\NUL\NUL\STX") @?= "\NUL\NUL\STX"
      render (parse "\NUL") @?= "\NUL"

  , testCase "equality ignores leading zero bytes" $ do
      parse "\NUL\STX" @?= parse "\STX"
      parse "\NUL\NUL" @?= empty
      assertBool "distinct" (parse "\STX" /= parse "\SOH")
      assertBool "distinct" (parse "\SOH\NUL" /= parse "\SOH")

  , testCase "show" $
      show (parse "\NUL\STX") @?= "parse \"\\NUL\\STX\""

  , testCase "bit numbering" $ do
      render (set_bit 0 empty) @?= "\SOH"
      render (set_bit 7 empty) @?= "\128"
      render (set_bit 8 empty) @?= "\SOH\NUL"
      render (set_bit 17 empty) @?= "\STX\NUL\NUL"
      set_bits (parse "\128\NUL\SOH") @?= [0, 23]

  , testCase "operations encode minimally" $ do
      render (set_bit 1 (parse "\NUL\NUL\SOH")) @?= "\ETX"
      render (clear_bit 0 (parse "\NUL\SOH\SOH")) @?= "\SOH\NUL"
      render (clear_bit 8 (parse "\SOH\SOH")) @?= "\SOH"
      render (union (parse "\NUL\NUL") empty) @?= ""

  , testCase "union" $ do
      union (parse "\SOH\NUL") (parse "\STX") @?= parse "\SOH\STX"
      union (parse "\STX") empty @?= parse "\STX"
      set_bits (union (parse "\SOH\SOH") (parse "\128\SOH")) @?= [0, 8, 15]

  , testCase "negative indices are ignored" $ do
      set_bit (-1) (parse "\STX") @?= parse "\STX"
      clear_bit (-1) (parse "\STX") @?= parse "\STX"
      test_bit (-1) (parse "\255") @?= False

  , testCase "clearing a bit beyond the vector is a no-op" $
      render (clear_bit 1000 (parse "\STX")) @?= "\STX"

  , testCase "bit 65536 does not wrap" $ do
      let fv = long_vector 8192 0x01
      set_bits fv @?= [65536]
      test_bit 65536 fv @?= True
      test_bit 0 fv @?= False

  , testCase "bits of a maximum-length vector" $ do
      let fv = long_vector 65534 0x80
      BS.length (render fv) @?= 65535
      set_bits fv @?= [524279]
      set_bit 524279 empty @?= fv
      clear_bit 524279 fv @?= empty
  ]

-- feature operations --------------------------------------------------------

feature_tests :: TestTree
feature_tests = testGroup "feature operations" [
    testCase "set_feature sets the bit for its level" $ do
      set_bits (set_feature BasicMpp Required empty) @?= [16]
      set_bits (set_feature BasicMpp Optional empty) @?= [17]

  , testCase "set_feature clears the other bit" $ do
      set_bits (set_feature PaymentSecret Optional (set_bit 14 empty))
        @?= [15]
      set_bits (set_feature PaymentSecret Required (set_bit 15 empty))
        @?= [14]

  , testCase "set_feature leaves other features alone" $
      set_bits (set_feature BasicMpp Optional (parse "\STX\NUL"))
        @?= [9, 17]

  , testCase "clear_feature clears both bits" $ do
      clear_feature BasicMpp (set_bit 16 (set_bit 17 empty)) @?= empty
      clear_feature BasicMpp (set_bit 9 (set_bit 17 empty))
        @?= set_bit 9 empty

  , testCase "test_feature" $ do
      test_feature BasicMpp (set_bit 16 empty) @?= Just Required
      test_feature BasicMpp (set_bit 17 empty) @?= Just Optional
      test_feature BasicMpp (set_bit 16 (set_bit 17 empty))
        @?= Just Required
      test_feature BasicMpp (set_bit 18 empty) @?= Nothing

  , testCase "list_features is in bit order and skips unknown bits" $
      list_features (set_bit 100 (set_bit 51 (set_bit 46 (set_bit 15 empty))))
        @?= [ (PaymentSecret, Optional), (OptionScidAlias, Required)
            , (OptionZeroconf, Optional) ]

  , testCase "set_feature_with_deps sets dependencies" $ do
      list_features (set_feature_with_deps OptionZeroconf Required empty)
        @?= [(OptionScidAlias, Required), (OptionZeroconf, Required)]
      list_features (set_feature_with_deps ZeroFeeCommitments Optional empty)
        @?= [(ZeroFeeCommitments, Optional), (OptionChannelType, Optional)]

  , testCase "set_feature_with_deps keeps a set dependency's level" $
      list_features
        (set_feature_with_deps OptionSimpleClose Optional
          (set_feature OptionShutdownAnysegwit Required empty))
        @?= [ (OptionShutdownAnysegwit, Required)
            , (OptionSimpleClose, Optional) ]
  ]

-- local validation ----------------------------------------------------------

-- | A vector with the given bits set.
bits :: [Int] -> FeatureVector
bits = foldr set_bit empty

contexts :: [Context]
contexts =
  [ InitContext, NodeContext, ChannelContext, InvoiceContext
  , BlindedContext, ChannelTypeContext ]

local_tests :: TestTree
local_tests = testGroup "validate_local" [
    testCase "empty is valid in every context but channel_type" $ do
      mapM_ (\c -> validate_local c empty @?= Right ())
            (filter (/= ChannelTypeContext) contexts)
      validate_local ChannelTypeContext empty
        @?= Left (InvalidChannelType :| [])

  , testCase "rejects unknown bits, odd or even" $ do
      validate_local InitContext (bits [20, 101])
        @?= Left (UnknownBit 20 :| [UnknownBit 101])
      validate_local NodeContext (bits [200]) @?= Left (UnknownBit 200 :| [])

  , testCase "rejects both bits set" $
      validate_local InitContext (bits [46, 47])
        @?= Left (BothBitsSet OptionScidAlias :| [])

  , testCase "rejects features outside their contexts" $ do
      validate_local InitContext (bits [49])
        @?= Left (ContextNotAllowed OptionPaymentMetadata InitContext :| [])
      validate_local InvoiceContext (bits [23])
        @?= Left (ContextNotAllowed OptionAnchors InvoiceContext :| [])
      validate_local ChannelContext (bits [17, 15])
        @?= Left (ContextNotAllowed PaymentSecret ChannelContext
                    :| [ContextNotAllowed BasicMpp ChannelContext])

  , testCase "accepts features in their contexts" $ do
      validate_local InvoiceContext (bits [49]) @?= Right ()
      validate_local NodeContext (bits [23, 47]) @?= Right ()
      validate_local InitContext (bits [63, 67, 39]) @?= Right ()

  , testCase "empty-context features: accepted in I, N and 9 only" $ do
      let fv = bits [1, 7, 9, 13, 15, 45]
      mapM_ (\c -> validate_local c fv @?= Right ())
            [InitContext, NodeContext, InvoiceContext]
      validate_local ChannelContext (bits [9])
        @?= Left (ContextNotAllowed VarOnionOptin ChannelContext :| [])
      validate_local BlindedContext (bits [15])
        @?= Left (ContextNotAllowed PaymentSecret BlindedContext :| [])

  , testCase "allowed_features must be empty" $
      validate_local BlindedContext (bits [17])
        @?= Left (ContextNotAllowed BasicMpp BlindedContext :| [])

  , testCase "rejects missing dependencies" $ do
      validate_local InitContext (bits [51])
        @?= Left (MissingDependency OptionZeroconf OptionScidAlias :| [])
      validate_local InitContext (bits [61])
        @?= Left (MissingDependency OptionSimpleClose
                    OptionShutdownAnysegwit :| [])
      validate_local NodeContext (bits [67])
        @?= Left (MissingDependency OptionOnionMessagesOnlyChannels
                    OptionOnionMessages :| [])

  , testCase "dependencies on ASSUMED features count as met" $ do
      validate_local InitContext (bits [17]) @?= Right ()
      validate_local InvoiceContext (bits [16]) @?= Right ()
      validate_local InitContext (bits [41]) @?= Right ()

  , testCase "reports errors in rule order" $
      validate_local InitContext (bits [20, 22, 23, 48, 61])
        @?= Left (UnknownBit 20 :|
              [ BothBitsSet OptionAnchors
              , ContextNotAllowed OptionPaymentMetadata InitContext
              , MissingDependency OptionSimpleClose
                  OptionShutdownAnysegwit ])

  , testCase "channel_type follows BOLT #2, not the generic rules" $ do
      validate_local ChannelTypeContext (bits [12, 50]) @?= Right ()
      validate_local ChannelTypeContext (bits [12, 23])
        @?= Left (InvalidChannelType :| [])
  ]

-- remote validation ---------------------------------------------------------

remote_tests :: TestTree
remote_tests = testGroup "validate_remote" [
    testCase "init: unknown odd bits are ignored" $
      validate_remote InitContext (bits [101, 201]) @?= Right ()

  , testCase "init: unknown even bits are rejected" $
      validate_remote InitContext (bits [20, 100, 101])
        @?= Left (UnknownBit 20 :| [UnknownBit 100])

  , testCase "init: missing dependencies are rejected" $ do
      validate_remote InitContext (bits [51])
        @?= Left (MissingDependency OptionZeroconf OptionScidAlias :| [])
      validate_remote InitContext (bits [60])
        @?= Left (MissingDependency OptionSimpleClose
                    OptionShutdownAnysegwit :| [])
      validate_remote InitContext (bits [51, 47]) @?= Right ()

  , testCase "init: ASSUMED dependencies count as met" $ do
      validate_remote InitContext (bits [17]) @?= Right ()
      validate_remote InitContext (bits [40]) @?= Right ()

  , testCase "init: both bits set is not an error" $
      validate_remote InitContext (bits [46, 47]) @?= Right ()

  , testCase "init: known features outside their contexts pass" $
      validate_remote InitContext (bits [49]) @?= Right ()

  , testCase "node and channel announcements: unknown even bits" $ do
      validate_remote NodeContext (bits [100])
        @?= Left (UnknownBit 100 :| [])
      validate_remote ChannelContext (bits [100])
        @?= Left (UnknownBit 100 :| [])
      validate_remote NodeContext (bits [101]) @?= Right ()
      validate_remote ChannelContext (bits [101]) @?= Right ()

  , testCase "node and channel announcements: no dependency rule" $ do
      validate_remote NodeContext (bits [51]) @?= Right ()
      validate_remote ChannelContext (bits [51]) @?= Right ()

  , testCase "invoice: unknown even bits and dependencies" $ do
      validate_remote InvoiceContext (bits [100])
        @?= Left (UnknownBit 100 :| [])
      validate_remote InvoiceContext (bits [51])
        @?= Left (MissingDependency OptionZeroconf OptionScidAlias :| [])
      validate_remote InvoiceContext (bits [101, 17]) @?= Right ()

  , testCase "blinded path: any unknown bit is rejected" $ do
      validate_remote BlindedContext (bits [101])
        @?= Left (UnknownBit 101 :| [])
      validate_remote BlindedContext (bits [100])
        @?= Left (UnknownBit 100 :| [])
      validate_remote BlindedContext (bits [15, 17]) @?= Right ()
      validate_remote BlindedContext empty @?= Right ()

  , testCase "channel_type: must be a defined type" $ do
      validate_remote ChannelTypeContext (bits [12, 22]) @?= Right ()
      validate_remote ChannelTypeContext (bits [22])
        @?= Left (InvalidChannelType :| [])

  , testCase "bits above 65535 do not wrap" $ do
      validate_remote InitContext (long_vector 8192 0x01)
        @?= Left (UnknownBit 65536 :| [])
      validate_remote InitContext (long_vector 8192 0x04)
        @?= Left (UnknownBit 65538 :| [])
      validate_remote InitContext (long_vector 8194 0x01)
        @?= Left (UnknownBit 65552 :| [])

  , testCase "maximum-length vectors" $ do
      validate_remote InitContext (long_vector 65534 0x80) @?= Right ()
      validate_remote InitContext (long_vector 65534 0x40)
        @?= Left (UnknownBit 524278 :| [])
  ]

-- BOLT #11 vectors ----------------------------------------------------------

-- | The feature vector carried by a BOLT #11 @9@ field, given the
--   field's data as bech32 characters.
invoice_features :: String -> Maybe FeatureVector
invoice_features cs = do
  vs <- traverse (`elemIndex` charset) cs
  let n = length vs
  pure $ bits
    [ (n - 1 - k) * 5 + j
    | (k, v) <- zip [0 ..] vs, j <- [0 .. 4], B.testBit v j ]
  where
    charset = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

invoice_case
  :: String -> String -> [Int]
  -> Either (NonEmpty ValidationError) () -> TestTree
invoice_case name dat expected result = testCase name $
  case invoice_features dat of
    Nothing -> assertFailure "invalid bech32 data"
    Just fv -> do
      set_bits fv @?= expected
      validate_remote InvoiceContext fv @?= result

invoice_tests :: TestTree
invoice_tests = testGroup "BOLT #11 invoice features" [
    invoice_case "features 8, 14 and 99" "sqqqqqqqqqqqqqqqqsgq"
      [8, 14, 99] (Right ())
  , invoice_case "payment metadata (8, 14, 48)" "gqqqqqqsgq"
      [8, 14, 48] (Right ())
  , invoice_case "high-S signature example (8, 14)" "sgq"
      [8, 14] (Right ())
  , invoice_case "invalid unknown feature 100" "psqqqqqqqqqqqqqqqqsgq"
      [8, 14, 99, 100] (Left (UnknownBit 100 :| []))
  , testCase "a writer may set 8, 14 and 48" $
      validate_local InvoiceContext (bits [8, 14, 48]) @?= Right ()
  ]

-- channel types -------------------------------------------------------------

all_channel_types :: [ChannelType]
all_channel_types =
  [ ChannelType b s z
  | b <- [BasicStaticRemotekey, BasicAnchors, BasicZeroFeeCommitments]
  , s <- [False, True]
  , z <- [False, True] ]

channel_type_tests :: TestTree
channel_type_tests = testGroup "channel types" [
    testCase "every defined type round-trips" $
      mapM_ (\ct -> channel_type (channel_type_features ct) @?= Just ct)
            all_channel_types

  , testCase "encodings" $ do
      let enc b s z = render (channel_type_features (ChannelType b s z))
      enc BasicStaticRemotekey False False @?= BS.pack [0x10, 0x00]
      enc BasicAnchors False False @?= BS.pack [0x40, 0x10, 0x00]
      enc BasicZeroFeeCommitments False False
        @?= BS.pack [0x01, 0, 0, 0, 0, 0]
      enc BasicAnchors True True
        @?= BS.pack [0x04, 0x40, 0, 0, 0x40, 0x10, 0x00]

  , testCase "exactly the twelve defined types decode" $ do
      let candidates = [12, 13, 14, 22, 23, 40, 46, 47, 50, 51, 52]
          decoded = [ ct | s <- subsequences candidates
                         , Just ct <- [channel_type (bits s)] ]
      length decoded @?= 12
      mapM_ (\ct -> assertBool (show ct) (ct `elem` all_channel_types))
            decoded

  , testCase "rejects undefined types" $
      mapM_ (\s -> channel_type (bits s) @?= Nothing)
        [ [], [13], [22], [23, 13], [12, 40], [12, 14], [46], [50]
        , [12, 47], [12, 51], [12, 52], [40, 44] ]

  , testCase "variations need no dependency closure" $
      channel_type (bits [12, 50])
        @?= Just (ChannelType BasicStaticRemotekey False True)

  , testCase "leading zero bytes are ignored" $
      channel_type (parse "\NUL\NUL\DLE\NUL")
        @?= Just (ChannelType BasicStaticRemotekey False False)
  ]

-- properties ----------------------------------------------------------------

gen_bytes :: Gen ByteString
gen_bytes = BS.pack <$> arbitrary

gen_vector :: Gen FeatureVector
gen_vector = parse <$> gen_bytes

gen_feature :: Gen Feature
gen_feature = elements known_features

gen_level :: Gen FeatureLevel
gen_level = elements [Required, Optional]

-- | A bit index somewhat beyond the end of a vector.
gen_index :: FeatureVector -> Gen Int
gen_index fv = choose (0, 8 * BS.length (render fv) + 16)

has_leading_zero :: FeatureVector -> Bool
has_leading_zero fv = BS.take 1 (render fv) == BS.singleton 0

properties :: TestTree
properties = testGroup "properties" [
    testProperty "render . parse == id" $
      forAll gen_bytes $ \bs -> render (parse bs) === bs

  , testProperty "equality ignores leading zero bytes" $
      forAll gen_bytes $ \bs (Small k) ->
        parse (BS.replicate k 0 <> bs) === parse bs

  , testProperty "equality agrees with set_bits" $
      forAll gen_vector $ \a -> forAll gen_vector $ \b ->
        (a == b) === (set_bits a == set_bits b)

  , testProperty "set_bits lists exactly the set bits, ascending" $
      forAll gen_vector $ \fv ->
        set_bits fv
          === filter (`test_bit` fv) [0 .. 8 * BS.length (render fv) - 1]

  , testProperty "set_bit sets only its bit" $
      forAll gen_vector $ \fv -> forAll (gen_index fv) $ \i ->
        let fv' = set_bit i fv
        in  test_bit i fv'
              .&&. filter (/= i) (set_bits fv')
                   === filter (/= i) (set_bits fv)

  , testProperty "clear_bit clears only its bit" $
      forAll gen_vector $ \fv -> forAll (gen_index fv) $ \i ->
        let fv' = clear_bit i fv
        in  not (test_bit i fv')
              .&&. set_bits fv' === filter (/= i) (set_bits fv)

  , testProperty "union is bitwise OR" $
      forAll gen_vector $ \a -> forAll gen_vector $ \b ->
        let u = union a b
            n = 8 * max (BS.length (render a)) (BS.length (render b))
        in  fmap (`test_bit` u) [0 .. n - 1]
              === fmap (\i -> test_bit i a || test_bit i b) [0 .. n - 1]

  , testProperty "operations encode minimally" $
      forAll gen_vector $ \a -> forAll gen_vector $ \b ->
      forAll (gen_index a) $ \i ->
        not (any has_leading_zero
              [union a b, set_bit i a, clear_bit i a])

  , testProperty "set_feature sets exactly one bit of the pair" $
      forAll gen_vector $ \fv -> forAll gen_feature $ \f ->
      forAll gen_level $ \l ->
        let fv'   = set_feature f l fv
            b     = feature_bit f
            other = filter (\i -> i /= b && i /= b + 1)
        in  test_feature f fv' === Just l
              .&&. test_bit b fv' =/= test_bit (b + 1) fv'
              .&&. other (set_bits fv') === other (set_bits fv)

  , testProperty "vectors built with set_feature_with_deps validate" $
      forAll (elements (filter (/= ChannelTypeContext) contexts)) $ \c ->
      forAll (gen_presented c) $ \fs ->
        let fv = foldr (\(f, l) -> set_feature_with_deps f l) empty fs
        in  validate_local c fv === Right ()
              .&&. validate_remote c fv === Right ()
  ]

-- | Features presented in a context, each with a level.
gen_presented :: Context -> Gen [(Feature, FeatureLevel)]
gen_presented c = do
  fs <- sublistOf (filter presented known_features)
  traverse (\f -> (,) f <$> gen_level) fs
  where
    presented f = case feature_contexts f of
      [] -> c `elem` [InitContext, NodeContext, InvoiceContext]
      cs -> c `elem` cs
