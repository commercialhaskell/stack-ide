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

import IdeSession

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data Options = Options {
    optInitParams :: SessionInitParams
  , optConfig     :: SessionConfig
  , optCommand    :: Command
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
  <$> parseInitParams
  <*> parseConfig
  <*> parseCommand

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

{-------------------------------------------------------------------------------
  Parsers for ide-backend types
-------------------------------------------------------------------------------}

parseInitParams :: Parser SessionInitParams
parseInitParams = SessionInitParams
    <$> useDefault sessionInitCabalMacros
    <*> (defaultTo sessionInitGhcOptions . many . strOption $ mconcat [
            long "ghc-option"
          , metavar "OPT"
          , help "GHC option to use when initializing the session. Can be used multiple times."
          ])
    <*> (defaultTo sessionInitRelativeIncludes . many . strOption $ mconcat [
            long "include"
          , metavar "DIR"
          , help "Include path (relative to session directory). Can be used multiple times."
          ])
    <*> useDefault sessionInitTargets
    <*> (defaultTo sessionInitRtsOpts . many . strOption $ mconcat [
            long "rts-option"
          , metavar "OPT"
          , help "RTS option to use when initializing the session. Can be used multiple times."
          ])
  where
    useDefault :: (SessionInitParams -> a) -> Parser a
    useDefault fld = pure (fld defaultSessionInitParams)

    defaultTo :: (SessionInitParams -> [a]) -> Parser [a] -> Parser [a]
    defaultTo fld = fmap $ \xs ->
      case xs of
        [] -> fld defaultSessionInitParams
        _  -> xs

parseConfig :: Parser SessionConfig
parseConfig = SessionConfig
    <$> (strOption $ mconcat [
            long "dir"
          , metavar "DIR"
          , value (configDir defaultSessionConfig)
          , showDefault
          , help "Directory to use to store the session files"
          ])
    <*> (defaultTo configExtraPathDirs . many . strOption $ mconcat [
            long "path"
          , metavar "DIR"
          , help "Additional directory to add to search for tools (ghc, ide-backend-server, etc.). Can be used multiple times."
          ])
    <*> useDefault configInProcess
    <*> useDefault configGenerateModInfo
    <*> useDefault configPackageDBStack
    <*> useDefault configLicenseExc
    <*> useDefault configLicenseFixed
    <*> useDefault configLog
    <*> (fmap not . switch $ mconcat [
            long "keep-tmp-files"
          , help "Do not delete the session directory on termination"
          ])
  where
    useDefault :: (SessionConfig -> a) -> Parser a
    useDefault fld = pure (fld defaultSessionConfig)

    defaultTo :: (SessionConfig -> [a]) -> Parser [a] -> Parser [a]
    defaultTo fld = fmap $ \xs ->
      case xs of
        [] -> fld defaultSessionConfig
        _  -> xs

{-------------------------------------------------------------------------------
  Top-level API
-------------------------------------------------------------------------------}

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

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

deriving instance Show SessionInitParams
deriving instance Show SessionConfig

instance Show (a -> b) where
  show _ = "<<function>>"
