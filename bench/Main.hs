module Main where

import Criterion.Main
import qualified Lightning.Protocol.BOLT9 as B9
import Fixtures

main :: IO ()
main = defaultMain [
    bgroup "vector" [
        bench "union" $ nf (B9.union typical) large
      , bench "set_bit" $ nf (B9.set_bit 101) typical
      , bench "clear_bit" $ nf (B9.clear_bit 17) typical
      , bench "test_bit" $ nf (B9.test_bit 17) typical
      , bench "set_bits" $ nf B9.set_bits typical
      , bench "== (leading zeros)" $ nf (== typical) padded
      ]

  , bgroup "feature" [
        bench "set_feature" $
          nf (B9.set_feature B9.OptionSplice B9.Optional) typical
      , bench "set_feature_with_deps" $
          nf (B9.set_feature_with_deps B9.OptionZeroconf B9.Optional)
             B9.empty
      , bench "test_feature" $ nf (B9.test_feature B9.BasicMpp) typical
      , bench "list_features" $ nf B9.list_features typical
      , bench "feature_by_bit" $ nf B9.feature_by_bit 51
      ]

  , bgroup "validate" [
        bench "validate_local (init)" $
          nf (B9.validate_local B9.InitContext) typical
      , bench "validate_remote (init)" $
          nf (B9.validate_remote B9.InitContext) typical
      , bench "validate_remote (init, 64 bytes)" $
          nf (B9.validate_remote B9.InitContext) large
      , bench "validate_remote (init, 65535 bytes)" $
          nf (B9.validate_remote B9.InitContext) maximal
      , bench "validate_remote (channel_type)" $
          nf (B9.validate_remote B9.ChannelTypeContext)
             zeroconf_type_features
      ]

  , bgroup "channel type" [
        bench "channel_type" $ nf B9.channel_type zeroconf_type_features
      , bench "channel_type_features" $
          nf B9.channel_type_features zeroconf_type
      ]
  ]
