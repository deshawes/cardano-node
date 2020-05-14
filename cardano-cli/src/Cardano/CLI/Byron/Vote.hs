{-# LANGUAGE GADTs #-}

module Cardano.CLI.Byron.Vote
  ( ByronVoteError(..)
  , renderByronVoteError
  , runVoteCreation
  , submitByronVote
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither)
import           Control.Tracer (stdoutTracer, traceWith)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text


import qualified Cardano.Binary as Binary
import           Cardano.Config.Protocol
                   (CardanoEra(..), RealPBFTError, renderRealPBFTError)
import           Cardano.Config.Types
import           Cardano.Chain.Genesis (GenesisData(..))
import           Cardano.Chain.Update
                   (AVote(..), UpId, Vote, mkVote, recoverUpId, recoverVoteId)
import           Cardano.CLI.Byron.UpdateProposal
                   (ByronUpdateProposalError, deserialiseByronUpdateProposal, readByronUpdateProposal)
import           Cardano.Crypto.Signing (SigningKey)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger.Mempool as Mempool
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Network.IOManager (IOManager)

import           Cardano.Api (Network)
import           Cardano.CLI.Byron.Genesis (ByronGenesisError, readGenesis)
import           Cardano.CLI.Byron.Tx (ByronTxError, nodeSubmitTx)
import           Cardano.CLI.Byron.Key (KeyFailure, readEraSigningKey)
import           Cardano.CLI.Helpers (HelpersError, ensureNewFileLBS)



data ByronVoteError
  = ByronVoteDecodingError !Binary.DecoderError
  | ByronReadVoteFileFailure !FilePath !Text
  | ByronVoteSubmissionError !RealPBFTError
  | ByronVoteKeyFailure !KeyFailure
  | ByronVoteUpdateProposalFailure !ByronUpdateProposalError
  | ByronVoteUpdateHelperError !HelpersError
  | ByronVoteGenesisError !ByronGenesisError
  | ByronVoteTxError !ByronTxError

renderByronVoteError :: ByronVoteError -> Text
renderByronVoteError bVerr =
  case bVerr of
    ByronVoteDecodingError decoderErr -> "Error decoding Byron vote: " <> (Text.pack $ show decoderErr)
    ByronReadVoteFileFailure fp err -> "Error reading Byron vote at " <> Text.pack fp <> " Error: " <> err
    ByronVoteSubmissionError realPBFTErr -> "Error submitting Byron vote: " <> renderRealPBFTError realPBFTErr
    _ -> panic "FILL ME IN LATER" --TODO: FILL ME IN


runVoteCreation
  :: ConfigYamlFilePath
  -> SigningKeyFile
  -> FilePath
  -> Bool
  -> FilePath
  -> ExceptT ByronVoteError IO ()
runVoteCreation configFp sKey upPropFp voteBool outputFp = do
  sK <- firstExceptT ByronVoteKeyFailure $ readEraSigningKey ByronEra sKey
  upProp <- firstExceptT ByronVoteUpdateProposalFailure $ readByronUpdateProposal upPropFp
  proposal <- hoistEither . first ByronVoteUpdateProposalFailure $ deserialiseByronUpdateProposal upProp
  let updatePropId = recoverUpId proposal
  vote <- createByronVote configFp sK updatePropId voteBool
  firstExceptT ByronVoteUpdateHelperError $ ensureNewFileLBS outputFp (serialiseByronVote vote)

convertVoteToGenTx :: AVote ByteString -> Mempool.GenTx ByronBlock
convertVoteToGenTx vote = Mempool.ByronUpdateVote (recoverVoteId vote) vote

createByronVote
  :: ConfigYamlFilePath
  -> SigningKey
  -> UpId
  -> Bool
  -> ExceptT ByronVoteError IO Vote
createByronVote config sKey upId voteChoice = do
  nc <- liftIO $ parseNodeConfigurationFP config
  (genData, _) <- firstExceptT ByronVoteGenesisError . readGenesis $ ncGenesisFile nc
  let pmId = gdProtocolMagicId genData
  --TODO: this reads the config file just to get the networkMagic
  pure $ mkVote pmId sKey upId voteChoice

deserialiseByronVote :: LByteString -> Either ByronVoteError (AVote ByteString)
deserialiseByronVote bs =
  case Binary.decodeFull bs of
    Left deserFail -> Left $ ByronVoteDecodingError deserFail
    Right vote -> Right $ annotateVote vote
 where
  annotateVote :: AVote Binary.ByteSpan -> AVote ByteString
  annotateVote vote = Binary.annotationBytes bs vote


serialiseByronVote :: Vote -> LByteString
serialiseByronVote = Binary.serialize

submitByronVote
  :: IOManager
  -> Network
  -> FilePath
  -> ExceptT ByronVoteError IO ()
submitByronVote iomgr network voteFp = do
    voteBs <- liftIO $ LB.readFile voteFp
    vote <- hoistEither $ deserialiseByronVote voteBs
    let genTx = convertVoteToGenTx vote
    traceWith stdoutTracer ("Vote TxId: " ++ condense (Mempool.txId genTx))
    firstExceptT ByronVoteTxError $ nodeSubmitTx iomgr network genTx
