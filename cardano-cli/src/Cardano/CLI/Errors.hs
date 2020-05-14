module Cardano.CLI.Errors
  ( CliError(..)
  ) where

import           Prelude (show)
import           Cardano.Prelude hiding (atomically, catch, option, show, unlines)

import qualified Data.Text as T

import           Cardano.Api (ApiError)
import           Cardano.Config.Shelley.Genesis (ShelleyGenesisError, renderShelleyGenesisError)
import           Cardano.Config.TextView (TextViewFileError, renderTextViewFileError)

import           Cardano.Config.Protocol
                   (CardanoEra, Protocol(..), ProtocolInstantiationError,
                    RealPBFTError, renderRealPBFTError, renderProtocolInstantiationError)
import           Cardano.Config.Shelley.Address (AddressError, renderAddressError)
import           Cardano.Config.Shelley.ColdKeys (KeyError, renderKeyError)
import           Cardano.Config.Shelley.KES (KESError, renderKESError)
import           Cardano.Config.Shelley.VRF (VRFError, renderVRFError)
import           Cardano.Config.Shelley.OCert (OperationalCertError)
import           Cardano.CLI.Byron.UpdateProposal (ByronUpdateProposalError)
import           Cardano.CLI.Byron.Vote (ByronVoteError, renderByronVoteError)
import           Cardano.CLI.Helpers (HelpersError, renderHelpersError)


-- | Exception type for all errors thrown by the CLI.
--   Well, almost all, since we don't rethrow the errors from readFile & such.
data CliError
  = AddressCliError AddressError
  | ByronVoteError !ByronVoteError
  | ByronUpdateProposalError !ByronUpdateProposalError
  | CardanoEraNotSupported !CardanoEra
  | GenerateTxsError !RealPBFTError
  | HelpersError !HelpersError

  | KESCliError KESError
  | KeyCliError KeyError
  | NoBlocksFound !FilePath
  | NodeSubmitTxError !RealPBFTError
  | NotEnoughTxInputs
  | NotEnoughTxOutputs
  | OperationalCertError OperationalCertError
  | ProtocolError !ProtocolInstantiationError

  | ShelleyCertReadError !ApiError
  -- TODO:  sadly, VerificationKeyParseError isn't exported from Cardano.Crypto.Signing/*
  | SpendGenesisUTxOError !RealPBFTError
  | VRFCliError VRFError
  | FileNotFoundError !FilePath
  | CardanoApiError !ApiError
  | IOError !FilePath !IOException
  | AesonDecode !FilePath !Text
  | ShelleyGenesisError !ShelleyGenesisError
  | IncorrectProtocolSpecifiedError !Protocol
  | AddressDescribeError !Text
  | CliTextViewFileError !TextViewFileError

instance Show CliError where
  show (AddressCliError e)
    = T.unpack $ renderAddressError e
  show (ByronUpdateProposalError e)
    =  show e --TODO: NEED ERROR RENDERING FUNCTION
  show (ByronVoteError e)
    = T.unpack $ renderByronVoteError e
  show (FileNotFoundError fp)
    = "File '" <> fp <> "' not found!"
  show (GenerateTxsError err)
    = "Error in GenerateTxs command: " <> (T.unpack $ renderRealPBFTError err)
  show (KESCliError err)
    = show $ renderKESError err
  show (KeyCliError err)
    = T.unpack $ renderKeyError err
  show (NoBlocksFound fp)
    = "Error while creating update proposal, no blocks found in: " <> fp
  show (NodeSubmitTxError err)
    = "Error in SubmitTx command: " <> (T.unpack $ renderRealPBFTError err)
  show NotEnoughTxInputs
    = "Transactions must have at least one input."
  show NotEnoughTxOutputs
    = "Transactions must have at least one output."
  show (HelpersError err)
    = T.unpack $ renderHelpersError err
  show (OperationalCertError err)
    = show err --TODO: renderOperationalCertError
  show (ProtocolError err)
    = "Protocol Instantiation Error " <> (T.unpack $ renderProtocolInstantiationError err)
  show (CardanoApiError apiError)
    = show apiError
  show (CardanoEraNotSupported era)
    = "Unsupported Cardano era " <> show era
  show (ShelleyCertReadError err)
    = "Shelley certificate read error: " <> show err
  show (SpendGenesisUTxOError err)
    = "Error in SpendGenesisUTxO command: " <> (T.unpack $ renderRealPBFTError err)
  show (VRFCliError err) = T.unpack $ renderVRFError err
  show (IOError fp ioe)
    = "File '" <> fp <> "': " ++ show ioe
  show (AesonDecode fp txt)
    = "File '" <> fp <> "': " ++ show txt
  show (ShelleyGenesisError sge)
    = T.unpack $ renderShelleyGenesisError sge
  show (IncorrectProtocolSpecifiedError ptcl)
    = "Incorrect protocol specified: " <> (toS $ show ptcl)
  show (AddressDescribeError txt)
    = T.unpack txt
  show (CliTextViewFileError err)
    = T.unpack $ renderTextViewFileError err
