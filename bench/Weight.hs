module Main where

import qualified Lightning.Protocol.BOLT9 as B9
import Weigh
import Fixtures

main :: IO ()
main = mainWith $ do
  func "union" (B9.union typical) large
  func "set_feature" (B9.set_feature B9.OptionSplice B9.Optional) typical
  func "set_feature_with_deps"
    (B9.set_feature_with_deps B9.OptionZeroconf B9.Optional) B9.empty
  func "list_features" B9.list_features typical
  func "validate_local (init)" (B9.validate_local B9.InitContext) typical
  func "validate_remote (init)" (B9.validate_remote B9.InitContext) typical
  func "validate_remote (init, 64 bytes)"
    (B9.validate_remote B9.InitContext) large
  func "channel_type" B9.channel_type zeroconf_type_features
