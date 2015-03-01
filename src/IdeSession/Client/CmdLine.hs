module IdeSession.Client.CmdLine (
    -- * Types
    Options(..)
  , Command(..)
  , EmptyOptions(..)
  , CabalOptions(..)
    -- * Parsing
  , getCommandLineOptions
  ) where

import Data.Monoid
import Options.Applicative

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data Options = Options {
    optCommand :: Command
  }
  deriving Show

data Command =
    StartEmptySession EmptyOptions
  | StartCabalSession CabalOptions
  | ShowAPI
  deriving Show

data EmptyOptions = EmptyOptions
  deriving Show

data CabalOptions = CabalOptions {
    cabalStanza :: Maybe String
  , cabalFile   :: FilePath
  }
  deriving Show

{-------------------------------------------------------------------------------
  Parsers
-------------------------------------------------------------------------------}

parseOptions :: Parser Options
parseOptions = Options
  <$> parseCommand

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
      command "empty" $ info
        (helper <*> (StartEmptySession <$> parseEmptyOptions))
        (progDesc "Start an empty session")
    , command "cabal" $ info
        (helper <*> (StartCabalSession <$> parseCabalOptions))
        (progDesc "Start a Cabal session")
    , command "show-api" $ info
        (helper <*> pure ShowAPI)
        (progDesc "Show the JSON API documentation")
    ]

parseEmptyOptions :: Parser EmptyOptions
parseEmptyOptions = pure EmptyOptions

parseCabalOptions :: Parser CabalOptions
parseCabalOptions = CabalOptions
    <$> (optional . strOption $ mconcat [
            long "stanza"
          , metavar "NAME"
          , help "Stanza in the Cabl file"
          ])
    <*> argument str (metavar "FILE")

getCommandLineOptions :: IO Options
getCommandLineOptions = execParser opts
  where
    opts = info
      (helper <*> parseOptions)
      (mconcat [
          fullDesc
        , progDesc "Start a new session"
        , header "ide-backend-client: JSON interface to ide-backend"
        ])
