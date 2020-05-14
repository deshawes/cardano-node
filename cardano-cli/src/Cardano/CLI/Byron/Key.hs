{-# LANGUAGE GeneralisedNewtypeDeriving #-}

{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Cardano.CLI.Byron.Key
  ( -- * Keys
    KeyFailure(..)
  , VerificationKeyFile(..)
  , NewSigningKeyFile(..)
  , NewVerificationKeyFile(..)
  , deserialiseSigningKey
  , prettyPublicKey
  , readEraSigningKey
  , readPaymentVerificationKey
  , serialisePoorKey
  , keygen
    -- * Passwords
  , PasswordRequirement(..)
  , PasswordPrompt
  , getPassphrase
  )
where

import           Prelude (String, show)
import           Cardano.Prelude hiding (option, show, trace, (%))

import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, handleIOExceptT, hoistEither)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as UTF8
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Formatting (build, (%), sformat)

import           System.IO (hSetEcho, hFlush, stdout, stdin)

import qualified Cardano.Chain.Common as Common
import qualified Cardano.Chain.Genesis as Genesis
import           Cardano.CLI.Helpers (HelpersError, serialiseSigningKey)
import qualified Cardano.CLI.Legacy.Byron as Legacy
import           Cardano.Config.Protocol (CardanoEra(..))
import           Cardano.Config.Types (SigningKeyFile(..))
import           Cardano.Crypto (SigningKey(..))
import qualified Cardano.Crypto.Random as Crypto
import qualified Cardano.Crypto.Signing as Crypto


data KeyFailure
  = ReadSigningKeyFailure !FilePath !Text
  | VerificationKeyDeserialisationFailed !FilePath !Text
  | SigningKeyDeserialisationFailed !FilePath !DeserialiseFailure
  | CardanoEraNotSupportedFailure !CardanoEra
  | ReadVerificationKeyFailure !FilePath !Text
  | HelpersError !HelpersError
  deriving Show


newtype NewSigningKeyFile =
  NewSigningKeyFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype VerificationKeyFile =
  VerificationKeyFile FilePath
  deriving (Eq, Ord, Show, IsString)

newtype NewVerificationKeyFile =
  NewVerificationKeyFile FilePath
   deriving (Eq, Ord, Show, IsString)

-- | Whether to require a password, or to supply an empty one.
data PasswordRequirement
  =  GetPassword
  |  EmptyPassword
  deriving (Eq, Show)

type PasswordPrompt = String

deserialiseSigningKey :: CardanoEra -> FilePath -> LB.ByteString
                      -> Either KeyFailure SigningKey
deserialiseSigningKey ByronEraLegacy fp delSkey =
  case deserialiseFromBytes Legacy.decodeLegacyDelegateKey delSkey of
    Left deSerFail -> Left $ SigningKeyDeserialisationFailed fp deSerFail
    Right (_, Legacy.LegacyDelegateKey sKey ) -> pure sKey

deserialiseSigningKey ByronEra fp delSkey =
  case deserialiseFromBytes Crypto.fromCBORXPrv delSkey of
    Left deSerFail -> Left $ SigningKeyDeserialisationFailed fp deSerFail
    Right (_, sKey) -> Right $ SigningKey sKey

deserialiseSigningKey ShelleyEra _ _ = Left $ CardanoEraNotSupportedFailure ShelleyEra


-- | Print some invariant properties of a public key:
--   its hash and formatted view.
prettyPublicKey :: Crypto.VerificationKey -> Text
prettyPublicKey vk =
  sformat (  "    public key hash: "% build %
           "\npublic key (base64): "% Crypto.fullVerificationKeyF %
           "\n   public key (hex): "% Crypto.fullVerificationKeyHexF)
    (Common.addressHash vk) vk vk

-- TODO:  we need to support password-protected secrets.
-- | Read signing key from a file.  Throw an error if the file can't be read or
-- fails to deserialise.
readEraSigningKey :: CardanoEra -> SigningKeyFile -> ExceptT KeyFailure IO SigningKey
readEraSigningKey era (SigningKeyFile fp) = do
  sK <- handleIOExceptT (ReadSigningKeyFailure fp . T.pack . displayException) $ LB.readFile fp

  -- Signing Key
  hoistEither $ deserialiseSigningKey era fp sK

-- | Read verification key from a file.  Throw an error if the file can't be read
-- or the key fails to deserialise.
readPaymentVerificationKey :: VerificationKeyFile -> ExceptT KeyFailure IO Crypto.VerificationKey
readPaymentVerificationKey (VerificationKeyFile fp) = do
  vkB <- handleIOExceptT (ReadVerificationKeyFailure fp . T.pack . displayException) (SB.readFile fp)
  -- Verification Key
  let eVk = hoistEither . Crypto.parseFullVerificationKey . fromString $ UTF8.toString vkB
  -- Convert error to 'CliError'
  firstExceptT (VerificationKeyDeserialisationFailed fp . T.pack . show) $ eVk


serialisePoorKey :: CardanoEra -> Genesis.PoorSecret
                 -> Either KeyFailure LB.ByteString
serialisePoorKey ByronEraLegacy ps = first HelpersError . serialiseSigningKey ByronEraLegacy $
                                       Genesis.poorSecretToKey ps
serialisePoorKey ByronEra       ps = first HelpersError . serialiseSigningKey ByronEra $
                                       Genesis.poorSecretToKey ps
serialisePoorKey ShelleyEra     _  = Left $ CardanoEraNotSupportedFailure ShelleyEra

-- | Generate a cryptographically random signing key,
--   protected with a (potentially empty) passphrase.
keygen :: Crypto.PassPhrase -> IO SigningKey
keygen passphrase =
  snd <$> Crypto.runSecureRandom (Crypto.safeKeyGen passphrase)

-- | Get a passphrase from the standard input,
--   depending on whether it's required.
getPassphrase :: PasswordPrompt -> PasswordRequirement -> IO Crypto.PassPhrase
getPassphrase desc GetPassword   = readPassword desc
getPassphrase _    EmptyPassword = pure Crypto.emptyPassphrase

-- | Obtain a 'Crypto.PassPhrase' from the standard input.
--   Terminal echoing is disabled.
readPassword :: String -> IO Crypto.PassPhrase
readPassword prompt =
  Crypto.PassPhrase . BA.convert <$> loop

  where
    loop :: IO ByteString
    loop = do
      (v1, v2) <- (,) <$> readOne prompt <*> readOne "Repeat to validate: "
      if v1 == v2
        then pure v1
        else hPutStrLn stdout ("Sorry, entered passwords don't match." :: String)
             >> loop

    readOne :: String -> IO ByteString
    readOne pr = do
      hPutStr stdout pr >> hFlush stdout
      hSetEcho stdout False
      pp <- SB.hGetLine stdin
      hSetEcho stdout True
      hPutStrLn stdout ("" :: String)
      pure pp
