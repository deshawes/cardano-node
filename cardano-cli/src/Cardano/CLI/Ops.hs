{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.CLI.Ops
  ( CardanoEra(..)
  , cardanoEraForProtocol
  , ncCardanoEra
  ) where

import           Cardano.Prelude hiding (atomically, catch, option)

import           Cardano.Config.Protocol (CardanoEra(..), cardanoEraForProtocol)
import           Cardano.Config.Types




ncCardanoEra :: NodeConfiguration -> CardanoEra
ncCardanoEra = cardanoEraForProtocol . ncProtocol
