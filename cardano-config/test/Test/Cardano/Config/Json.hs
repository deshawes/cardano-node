{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cardano.Config.Json
  ( tests
  ) where

import           Cardano.Prelude
import           Cardano.Config.Types

import           Data.Aeson (encode, fromJSON, decode, toJSON)

import           Hedgehog (Property, discover)
import qualified Hedgehog

import           Test.Cardano.Config.Gen

prop_roundtrip_NodeAddress_JSON :: Property
prop_roundtrip_NodeAddress_JSON =
  Hedgehog.property $ do
    na <- Hedgehog.forAll genNodeAddress
    Hedgehog.tripping na toJSON fromJSON
    Hedgehog.tripping na encode decode
    Hedgehog.cover 1 "  has address" $ isJust (unNodeHostAddress $ naHostAddress na)
    Hedgehog.cover 1 "  no address" $ isNothing (unNodeHostAddress $ naHostAddress na)

prop_roundtrip_NodeHostAddress_JSON :: Property
prop_roundtrip_NodeHostAddress_JSON =
  Hedgehog.property $ do
    nha <- Hedgehog.forAll genNodeHostAddress
    Hedgehog.tripping nha toJSON fromJSON
    Hedgehog.tripping nha encode decode

prop_roundtrip_NodeSetup_JSON :: Property
prop_roundtrip_NodeSetup_JSON =
  Hedgehog.property $ do
    ns <- Hedgehog.forAll genNodeSetup
    Hedgehog.tripping ns toJSON fromJSON
    Hedgehog.tripping ns encode decode

prop_roundtrip_NetworkTopology_JSON :: Property
prop_roundtrip_NetworkTopology_JSON =
  Hedgehog.property $ do
    ntop <- Hedgehog.forAll genNetworkTopology
    Hedgehog.tripping ntop toJSON fromJSON
    Hedgehog.tripping ntop encode decode


-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  Hedgehog.checkParallel $$discover
