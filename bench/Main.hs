module Main where

import Criterion.Main
import Data.Maybe (fromJust)
import qualified Lightning.Protocol.BOLT9 as B9
import Fixtures

basic_mpp :: B9.Feature
basic_mpp = fromJust (B9.featureByName "basic_mpp")

main :: IO ()
main = defaultMain [
    bgroup "parse" [
        bench "typical (8 bytes)"  $ nf B9.parse typicalBytes
      , bench "empty"              $ nf B9.parse emptyBytes
      , bench "large (64 bytes)"   $ nf B9.parse largeBytes
      ]

  , bgroup "render" [
        bench "typical"  $ nf B9.render typicalFV
      , bench "empty"    $ nf B9.render emptyFV
      ]

  , bgroup "bit-ops" [
        bench "setBit"   $ nf (B9.setBit 50) typicalFV
      , bench "testBit"  $ nf (B9.testBit 17) typicalFV
      , bench "hasFeature" $
          nf (flip B9.hasFeature typicalFV) basic_mpp
      ]

  , bgroup "validate" [
        bench "validateLocal (valid)"   $
          nf (B9.validateLocal B9.Init) validFV
      , bench "validateRemote (unknown bits)" $
          nf (B9.validateRemote B9.Init) unknownBitsFV
      ]

  , bgroup "lookup" [
        bench "featureByBit"  $ nf B9.featureByBit 16
      , bench "featureByName" $ nf B9.featureByName "basic_mpp"
      ]
  ]
