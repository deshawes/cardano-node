module Cardano.CLI.Byron.Run
  ( runByronClientCommand
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (hoistEither, firstExceptT)
import           Data.Semigroup ((<>))
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Formatting as F

import           Ouroboros.Network.NodeToClient (IOManager, withIOManager)

import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Delegation as Delegation
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.Chain.Slotting (EpochNumber)
import           Cardano.Chain.UTxO (TxIn, TxOut)

import qualified Cardano.Crypto.Hashing as Crypto
import qualified Cardano.Crypto.Signing as Crypto

import           Cardano.Config.Protocol (RealPBFTError)
import           Cardano.Config.Types

import           Cardano.Api (Network, toByronNetworkMagic)
import           Cardano.CLI.Byron.Commands
import           Cardano.CLI.Byron.Delegation
import           Cardano.CLI.Byron.Genesis
import           Cardano.CLI.Byron.Key
import           Cardano.CLI.Byron.Query
import           Cardano.CLI.Byron.Tx
import           Cardano.CLI.Byron.UpdateProposal
import           Cardano.CLI.Byron.Vote (ByronVoteError, runVoteCreation, submitByronVote)

import           Cardano.CLI.Helpers
import           Cardano.CLI.Ops

data ByronClientCommandError
  = ByronGenesisCommand !ByronGenesisError
  | ByronCommandHelpersError !HelpersError
  | ByronCommandRealPBFTError !RealPBFTError
  | ByronCommandKeyFailure !KeyFailure
  | ByronCommandTxError !ByronTxError
  | ByronCommandDelegationFailure !ByronDelegationError
  | ByronCommandUpdateProposalFailure !ByronUpdateProposalError
  | ByronCommandVoteFailure !ByronVoteError
  | ByronCommandQueryFailure !ByronQueryError

runByronClientCommand :: ByronCommand -> ExceptT ByronClientCommandError IO ()
runByronClientCommand c =
  case c of
    NodeCmd bc -> runNodeCmd bc
    Genesis outDir params era -> runGenesisCommand outDir params era
    GetLocalNodeTip network -> firstExceptT ByronCommandQueryFailure $ runGetLocalNodeTip network
    ValidateCBOR cborObject fp -> runValidateCBOR cborObject fp
    PrettyPrintCBOR fp -> runPrettyPrintCBOR fp
    PrettySigningKeyPublic era skF -> runPrettySigningKeyPublic era skF
    MigrateDelegateKeyFrom oldEra oldKey newEra nskf -> runMigrateDelegateKeyFrom oldEra oldKey newEra nskf
    PrintGenesisHash genFp -> runPrintGenesisHash genFp
    PrintSigningKeyAddress era netMagic skF -> runPrintSigningKeyAddress era netMagic skF
    Keygen era nskf passReq -> runKeygen era nskf passReq
    ToVerification era skFp nvkFp -> runToVerification era skFp nvkFp
    IssueDelegationCertificate configFp epoch issuerSK delVK cert -> runIssueDelegationCertificate configFp epoch issuerSK delVK cert
    CheckDelegation configFp cert issuerVF delegateVF -> runCheckDelegation configFp cert issuerVF delegateVF
    SubmitTx network fp -> runSubmitTx network fp
    SpendGenesisUTxO configFp nftx ctKey genRichAddr outs -> runSpendGenesisUTxO configFp nftx ctKey genRichAddr outs
    SpendUTxO configFp nftx ctKey ins outs -> runSpendUTxO configFp nftx ctKey ins outs


runNodeCmd :: NodeCmd -> ExceptT ByronClientCommandError IO ()
runNodeCmd (CreateVote configFp sKey upPropFp voteBool outputFp) =
  firstExceptT ByronCommandVoteFailure $ runVoteCreation configFp sKey upPropFp voteBool outputFp

runNodeCmd (SubmitUpdateProposal network proposalFp) =
  withIOManagerE $ \iomgr -> firstExceptT ByronCommandUpdateProposalFailure $ submitByronUpdateProposal iomgr network proposalFp

runNodeCmd (SubmitVote network voteFp) =
  withIOManagerE $ \iomgr -> firstExceptT ByronCommandVoteFailure $ submitByronVote iomgr network voteFp

runNodeCmd (UpdateProposal configFp sKey pVer sVer sysTag insHash outputFp params) =
  firstExceptT ByronCommandUpdateProposalFailure $ runProposalCreation configFp sKey pVer sVer sysTag insHash outputFp params

runGenesisCommand :: NewDirectory -> GenesisParameters -> CardanoEra -> ExceptT ByronClientCommandError IO ()
runGenesisCommand outDir params era = do
  (genData, genSecrets) <- firstExceptT ByronGenesisCommand $ mkGenesis params
  firstExceptT ByronGenesisCommand $ dumpGenesis era outDir genData genSecrets

runValidateCBOR :: CBORObject -> FilePath -> ExceptT ByronClientCommandError IO ()
runValidateCBOR cborObject fp = do
  bs <- firstExceptT ByronCommandHelpersError $ readCBOR fp
  res <- hoistEither . first ByronCommandHelpersError $ validateCBOR cborObject bs
  liftIO $ putTextLn res

runPrettyPrintCBOR :: FilePath -> ExceptT ByronClientCommandError IO ()
runPrettyPrintCBOR fp = do
  bs <- firstExceptT ByronCommandHelpersError $ readCBOR fp
  firstExceptT ByronCommandHelpersError $ pPrintCBOR bs

runPrettySigningKeyPublic :: CardanoEra -> SigningKeyFile -> ExceptT ByronClientCommandError IO ()
runPrettySigningKeyPublic era skF = do
  sK <- firstExceptT ByronCommandKeyFailure $ readEraSigningKey era skF
  liftIO . putTextLn . prettyPublicKey $ Crypto.toVerification sK

runMigrateDelegateKeyFrom
        :: CardanoEra -> SigningKeyFile -> CardanoEra -> NewSigningKeyFile
        -> ExceptT ByronClientCommandError IO ()
runMigrateDelegateKeyFrom oldEra oldKey newEra (NewSigningKeyFile newKey) = do
  sk <- firstExceptT ByronCommandKeyFailure $ readEraSigningKey oldEra oldKey
  sDk <- hoistEither . first ByronCommandDelegationFailure $ serialiseDelegateKey newEra sk
  firstExceptT ByronCommandHelpersError $ ensureNewFileLBS newKey sDk

runPrintGenesisHash :: GenesisFile -> ExceptT ByronClientCommandError IO ()
runPrintGenesisHash genFp = do
    gen <- firstExceptT ByronGenesisCommand $ readGenesis genFp
    liftIO . putTextLn $ formatter gen
  where
    formatter :: (a, Genesis.GenesisHash)-> Text
    formatter = F.sformat Crypto.hashHexF . Genesis.unGenesisHash . snd

runPrintSigningKeyAddress :: CardanoEra -> Network -> SigningKeyFile -> ExceptT ByronClientCommandError IO ()
runPrintSigningKeyAddress era network skF = do
  sK <- firstExceptT ByronCommandKeyFailure $ readEraSigningKey era skF
  let sKeyAddress = prettyAddress
                  . Common.makeVerKeyAddress (toByronNetworkMagic network)
                  . Crypto.toVerification
                  $ sK
  liftIO $ putTextLn sKeyAddress

runKeygen :: CardanoEra -> NewSigningKeyFile -> PasswordRequirement -> ExceptT ByronClientCommandError IO ()
runKeygen era (NewSigningKeyFile skF) passReq = do
  pPhrase <- liftIO $ getPassphrase ("Enter password to encrypt '" <> skF <> "': ") passReq
  sK <- liftIO $ keygen pPhrase
  serDk <- hoistEither . first ByronCommandDelegationFailure $ serialiseDelegateKey era sK
  firstExceptT ByronCommandHelpersError $ ensureNewFileLBS skF serDk

runToVerification :: CardanoEra -> SigningKeyFile -> NewVerificationKeyFile -> ExceptT ByronClientCommandError IO ()
runToVerification era skFp (NewVerificationKeyFile vkFp) = do
  sk <- firstExceptT ByronCommandKeyFailure $ readEraSigningKey era skFp
  let vKey = Builder.toLazyText . Crypto.formatFullVerificationKey $ Crypto.toVerification sk
  firstExceptT ByronCommandHelpersError $ ensureNewFile TL.writeFile vkFp vKey

runIssueDelegationCertificate
        :: ConfigYamlFilePath -> EpochNumber -> SigningKeyFile -> VerificationKeyFile -> NewCertificateFile
        -> ExceptT ByronClientCommandError IO ()
runIssueDelegationCertificate configFp epoch issuerSK delegateVK cert = do
  nc <- liftIO $ parseNodeConfigurationFP configFp
  vk <- firstExceptT ByronCommandKeyFailure $ readPaymentVerificationKey delegateVK
  sk <- firstExceptT ByronCommandKeyFailure $ readEraSigningKey (ncCardanoEra nc) issuerSK
  pmId <- firstExceptT ByronGenesisCommand . readProtocolMagicId $ ncGenesisFile nc
  let byGenDelCert :: Delegation.Certificate
      byGenDelCert = issueByronGenesisDelegation pmId epoch sk vk
  sCert <- hoistEither . first ByronCommandDelegationFailure $ serialiseDelegationCert (ncCardanoEra nc) byGenDelCert
  firstExceptT ByronCommandHelpersError $ ensureNewFileLBS (nFp cert) sCert


runCheckDelegation
        :: ConfigYamlFilePath -> CertificateFile -> VerificationKeyFile -> VerificationKeyFile
        -> ExceptT ByronClientCommandError IO ()
runCheckDelegation configFp cert issuerVF delegateVF = do
  nc <- liftIO $ parseNodeConfigurationFP configFp
  issuerVK <- firstExceptT ByronCommandKeyFailure $ readPaymentVerificationKey issuerVF
  delegateVK <- firstExceptT ByronCommandKeyFailure $ readPaymentVerificationKey delegateVF
  pmId <- firstExceptT ByronGenesisCommand $ readProtocolMagicId $ ncGenesisFile nc
  firstExceptT ByronCommandDelegationFailure $ checkByronGenesisDelegation cert pmId issuerVK delegateVK

runSubmitTx :: Network -> TxFile -> ExceptT ByronClientCommandError IO ()
runSubmitTx network fp =
  withIOManagerE $ \iomgr -> do
    tx <- firstExceptT ByronCommandTxError $ readByronTx fp
    firstExceptT ByronCommandTxError $ nodeSubmitTx iomgr network tx


runSpendGenesisUTxO
        :: ConfigYamlFilePath -> NewTxFile -> SigningKeyFile -> Common.Address -> NonEmpty TxOut
        -> ExceptT ByronClientCommandError IO ()
runSpendGenesisUTxO configFp (NewTxFile ctTx) ctKey genRichAddr outs = do
    nc <- liftIO $ parseNodeConfigurationFP configFp
    sk <- firstExceptT ByronCommandKeyFailure $ readEraSigningKey (ncCardanoEra nc) ctKey

    tx <- firstExceptT ByronCommandRealPBFTError $
            issueGenesisUTxOExpenditure nc genRichAddr outs sk
    firstExceptT ByronCommandHelpersError $ ensureNewFileLBS ctTx $ toCborTxAux tx

runSpendUTxO
        :: ConfigYamlFilePath -> NewTxFile -> SigningKeyFile -> NonEmpty TxIn -> NonEmpty TxOut
        -> ExceptT ByronClientCommandError IO ()
runSpendUTxO configFp (NewTxFile ctTx) ctKey ins outs = do
    nc <- liftIO $ parseNodeConfigurationFP configFp
    sk <- firstExceptT ByronCommandKeyFailure $ readEraSigningKey (ncCardanoEra nc) ctKey

    gTx <- firstExceptT ByronCommandRealPBFTError $
             issueUTxOExpenditure nc ins outs sk
    firstExceptT ByronCommandHelpersError . ensureNewFileLBS ctTx $ toCborTxAux gTx

withIOManagerE :: (IOManager -> ExceptT e IO a) -> ExceptT e IO a
withIOManagerE k = ExceptT $ withIOManager (runExceptT . k)
