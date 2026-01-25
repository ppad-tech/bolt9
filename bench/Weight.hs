module Main where

import Weigh
import qualified Lightning.Protocol.BOLT9 as B9
import Fixtures

main :: IO ()
main = mainWith $ do
  func "FeatureVector (5 features)" mkFiveFeatures ()
  func "validateLocal" (B9.validateLocal B9.Init) validFV
  func "listFeatures" B9.listFeatures validFV

-- | Create a FeatureVector with 5 features set.
mkFiveFeatures :: () -> B9.FeatureVector
mkFiveFeatures _ =
    B9.setBit 15   -- payment_secret
  $ B9.setBit 17   -- basic_mpp
  $ B9.setBit 23   -- option_anchors
  $ B9.setBit 25   -- option_route_blinding
  $ B9.setBit 27   -- option_shutdown_anysegwit
  $ B9.empty
