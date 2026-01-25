{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString as BS
import Data.Maybe (isJust, isNothing)
import Data.Word (Word16)
import Lightning.Protocol.BOLT9
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ppad-bolt9" [
    featureTableTests
  , bitParityTests
  , featureVectorTests
  , validationTests
  , propertyTests
  ]

-- Feature table tests ---------------------------------------------------------

featureTableTests :: TestTree
featureTableTests = testGroup "Feature table" [
    testCase "all features have even baseBit" $
      all (even . featureBaseBit) knownFeatures @?= True

  , testCase "featureByBit works for even bit" $
      case featureByBit 14 of
        Nothing -> assertFailure "expected to find payment_secret"
        Just f  -> featureName f @?= "payment_secret"

  , testCase "featureByBit works for odd bit" $
      case featureByBit 15 of
        Nothing -> assertFailure "expected to find payment_secret"
        Just f  -> featureName f @?= "payment_secret"

  , testCase "featureByBit returns Nothing for unknown bit" $
      featureByBit 100 @?= Nothing

  , testCase "featureByName finds known features" $ do
      isJust (featureByName "var_onion_optin") @?= True
      isJust (featureByName "payment_secret") @?= True
      isJust (featureByName "basic_mpp") @?= True

  , testCase "featureByName returns Nothing for unknown names" $
      featureByName "nonexistent_feature" @?= Nothing

  , testCase "assumed features are present" $ do
      let assumedNames = [ "option_data_loss_protect"
                         , "var_onion_optin"
                         , "option_static_remotekey"
                         , "payment_secret"
                         , "option_channel_type"
                         ]
      mapM_ (\n -> isJust (featureByName n) @?
               ("assumed feature missing: " ++ n)) assumedNames

  , testCase "assumed features are marked as assumed" $ do
      let checkAssumed n = case featureByName n of
            Nothing -> assertFailure $ "feature not found: " ++ n
            Just f  -> featureAssumed f @?= True
      mapM_ checkAssumed [ "option_data_loss_protect"
                         , "var_onion_optin"
                         , "option_static_remotekey"
                         , "payment_secret"
                         , "option_channel_type"
                         ]
  ]

-- Bit parity tests ------------------------------------------------------------

bitParityTests :: TestTree
bitParityTests = testGroup "Bit parity" [
    testCase "requiredBit accepts even numbers" $ do
      isJust (requiredBit 0) @?= True
      isJust (requiredBit 2) @?= True
      isJust (requiredBit 100) @?= True

  , testCase "requiredBit rejects odd numbers" $ do
      isNothing (requiredBit 1) @?= True
      isNothing (requiredBit 3) @?= True
      isNothing (requiredBit 101) @?= True

  , testCase "optionalBit accepts odd numbers" $ do
      isJust (optionalBit 1) @?= True
      isJust (optionalBit 3) @?= True
      isJust (optionalBit 101) @?= True

  , testCase "optionalBit rejects even numbers" $ do
      isNothing (optionalBit 0) @?= True
      isNothing (optionalBit 2) @?= True
      isNothing (optionalBit 100) @?= True

  , testCase "requiredBit smart constructor preserves value" $
      case requiredBit 42 of
        Nothing -> assertFailure "expected Just"
        Just rb -> unRequiredBit rb @?= 42

  , testCase "optionalBit smart constructor preserves value" $
      case optionalBit 43 of
        Nothing -> assertFailure "expected Just"
        Just ob -> unOptionalBit ob @?= 43
  ]

-- FeatureVector tests ---------------------------------------------------------

featureVectorTests :: TestTree
featureVectorTests = testGroup "FeatureVector" [
    testCase "empty has no bits set" $ do
      unFeatureVector empty @?= BS.empty
      member (bitIndex 0) empty @?= False
      member (bitIndex 100) empty @?= False

  , testCase "set adds a bit" $ do
      let fv = set (bitIndex 0) empty
      member (bitIndex 0) fv @?= True

  , testCase "set multiple bits" $ do
      let fv = set (bitIndex 8) (set (bitIndex 0) empty)
      member (bitIndex 0) fv @?= True
      member (bitIndex 8) fv @?= True
      member (bitIndex 1) fv @?= False

  , testCase "clear removes a bit" $ do
      let fv  = set (bitIndex 5) empty
          fv' = clear (bitIndex 5) fv
      member (bitIndex 5) fv @?= True
      member (bitIndex 5) fv' @?= False

  , testCase "clear on unset bit is no-op" $ do
      let fv = clear (bitIndex 10) empty
      unFeatureVector fv @?= BS.empty

  , testCase "member returns False for unset bits" $ do
      let fv = set (bitIndex 4) empty
      member (bitIndex 0) fv @?= False
      member (bitIndex 1) fv @?= False
      member (bitIndex 5) fv @?= False

  , testCase "render strips leading zeros" $ do
      let fv = set (bitIndex 0) empty
      render fv @?= BS.pack [0x01]

  , testCase "render handles high bits correctly" $ do
      let fv = set (bitIndex 15) empty
      render fv @?= BS.pack [0x80, 0x00]

  , testCase "render of empty is empty" $
      render empty @?= BS.empty
  ]

-- Validation tests ------------------------------------------------------------

validationTests :: TestTree
validationTests = testGroup "Validation" [
    testCase "BothBitsSet error when both bits of a pair are set" $ do
      let baseBit = 14  -- payment_secret
          fv = setBit (baseBit + 1) (setBit baseBit empty)
      case validateLocal Init fv of
        Right () -> assertFailure "expected BothBitsSet error"
        Left errs -> any isBothBitsSet errs @?= True

  , testCase "MissingDependency error when dep not set" $ do
      -- basic_mpp (16) depends on payment_secret (14)
      case featureByName "basic_mpp" of
        Nothing -> assertFailure "basic_mpp not found"
        Just f  -> do
          let fv = setBit (featureBaseBit f) empty
          case validateLocal Init fv of
            Right () -> assertFailure "expected MissingDependency error"
            Left errs -> any isMissingDependency errs @?= True

  , testCase "ContextNotAllowed error for wrong context" $ do
      -- option_payment_metadata (48) is only allowed in Invoice context
      case featureByName "option_payment_metadata" of
        Nothing -> assertFailure "option_payment_metadata not found"
        Just f  -> do
          let fv = setBit (featureBaseBit f) empty
          case validateLocal Init fv of
            Right () -> assertFailure "expected ContextNotAllowed error"
            Left errs -> any isContextNotAllowed errs @?= True

  , testCase "Remote validation accepts unknown optional bits" $ do
      -- bit 201 is unknown and odd (optional)
      let fv = setBit 201 empty
      validateRemote Init fv @?= Right ()

  , testCase "Remote validation rejects unknown required bits" $ do
      -- bit 200 is unknown and even (required)
      let fv = setBit 200 empty
      case validateRemote Init fv of
        Right () -> assertFailure "expected UnknownRequiredBit error"
        Left errs -> any isUnknownRequiredBit errs @?= True

  , testCase "Valid local vector passes validation" $ do
      -- payment_secret (14) with its required bit set
      case featureByName "payment_secret" of
        Nothing -> assertFailure "payment_secret not found"
        Just f  -> do
          let fv = setBit (featureBaseBit f) empty
          validateLocal Init fv @?= Right ()

  , testCase "Valid feature with dependency passes" $ do
      -- basic_mpp (16) with payment_secret (14) dependency set
      case (featureByName "basic_mpp", featureByName "payment_secret") of
        (Just mpp, Just ps) -> do
          let fv = setBit (featureBaseBit mpp)
                 $ setBit (featureBaseBit ps) empty
          validateLocal Init fv @?= Right ()
        _ -> assertFailure "features not found"
  ]

isBothBitsSet :: ValidationError -> Bool
isBothBitsSet (BothBitsSet _ _) = True
isBothBitsSet _ = False

isMissingDependency :: ValidationError -> Bool
isMissingDependency (MissingDependency _ _) = True
isMissingDependency _ = False

isContextNotAllowed :: ValidationError -> Bool
isContextNotAllowed (ContextNotAllowed _ _) = True
isContextNotAllowed _ = False

isUnknownRequiredBit :: ValidationError -> Bool
isUnknownRequiredBit (UnknownRequiredBit _) = True
isUnknownRequiredBit _ = False

-- Property tests --------------------------------------------------------------

propertyTests :: TestTree
propertyTests = testGroup "Properties" [
    testProperty "render . parse == id for stripped ByteStrings" $
      \bs -> let stripped = BS.dropWhile (== 0) (BS.pack bs)
             in  render (parse stripped) === stripped

  , testProperty "set then member returns True" $
      \(Small n) -> let idx = bitIndex (n `mod` 256)
                        fv  = set idx empty
                    in  member idx fv === True

  , testProperty "clear then member returns False" $
      \(Small n) -> let idx = bitIndex (n `mod` 256)
                        fv  = set idx empty
                        fv' = clear idx fv
                    in  member idx fv' === False

  , testProperty "setBits returns all set bits without duplicates" $
      \bs -> let fv   = parse (BS.pack bs)
                 bits = setBits fv
                 -- Verify no duplicates and length matches unique count
             in  length bits === length (removeDups bits)

  , testProperty "double set is idempotent" $
      \(Small n) -> let idx = bitIndex (n `mod` 256)
                        fv  = set idx empty
                        fv' = set idx fv
                    in  unFeatureVector fv === unFeatureVector fv'

  , testProperty "set then clear restores original (when was unset)" $
      \(Small n) -> let idx = bitIndex (n `mod` 256)
                        fv  = set idx empty
                        fv' = clear idx fv
                    in  unFeatureVector fv' === BS.empty

  , testProperty "member consistent with setBits" $
      \bs -> let fv   = parse (BS.pack bs)
                 bits = setBits fv
             in  all (\b -> member (bitIndex b) fv) bits === True

  , testProperty "requiredBit only succeeds for even" $
      \(n :: Word16) -> isJust (requiredBit n) === (n `mod` 2 == 0)

  , testProperty "optionalBit only succeeds for odd" $
      \(n :: Word16) -> isJust (optionalBit n) === (n `mod` 2 == 1)
  ]

-- | Remove duplicates from a list.
removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = x : removeDups (filter (/= x) xs)
