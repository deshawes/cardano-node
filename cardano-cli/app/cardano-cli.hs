{-# LANGUAGE OverloadedStrings #-}

import           Cardano.Prelude hiding (option)

import           Control.Monad.Trans.Except.Exit (orDie)
import qualified Data.Text as Text
import qualified Options.Applicative as Opt
import           Options.Applicative (ParserInfo, ParserPrefs, showHelpOnEmpty)

import           Cardano.CLI.Errors
import           Cardano.CLI.Parsers (ClientCommand, parseClientCommand)
import           Cardano.CLI.Run (runClientCommand)
import           Cardano.Config.TopHandler


main :: IO ()
main = toplevelExceptionHandler $ do

  co <- Opt.customExecParser pref opts

  orDie renderCliError $ runClientCommand co

  where
    pref :: ParserPrefs
    pref = Opt.prefs showHelpOnEmpty

    opts :: ParserInfo ClientCommand
    opts =
      Opt.info (parseClientCommand <**> Opt.helper)
        ( Opt.fullDesc
          <> Opt.header
          "cardano-cli - utility to support a variety of key\
          \ operations (genesis generation, migration,\
          \ pretty-printing..) for different system generations."
        )

    renderCliError :: CliError -> Text
    renderCliError = Text.pack . show
