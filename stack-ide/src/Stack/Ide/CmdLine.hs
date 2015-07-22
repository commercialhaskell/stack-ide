{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Stack.Ide.CmdLine (
    -- * Types
    Options(..)
  , EmptyOptions(..)
  , CabalOptions(..)
    -- * Parsing
  , getCommandLineOptions
  ) where

import Data.Monoid
import Options.Applicative
import System.Directory (getHomeDirectory)
import System.FilePath
import System.IO.Unsafe (unsafePerformIO)
import qualified Options.Applicative.Help.Chunk as Chunk
import qualified Text.PrettyPrint.ANSI.Leijen   as Doc

import IdeSession

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

data Options = Options {
    optInitParams :: SessionInitParams
  , optConfig     :: SessionConfig
  }


data EmptyOptions = EmptyOptions
  deriving Show

data CabalOptions = CabalOptions {
    cabalTarget :: String
  , cabalRoot   :: FilePath
  }
  deriving Show

{-------------------------------------------------------------------------------
  Parsers
-------------------------------------------------------------------------------}

parseOptions :: Parser Options
parseOptions = Options
  <$> parseInitParams
  <*> parseConfig


{-------------------------------------------------------------------------------
  Parsers for ide-backend types
-------------------------------------------------------------------------------}

parseInitParams :: Parser SessionInitParams
parseInitParams = SessionInitParams
    <$> useDefault sessionInitCabalMacros
    <*> (defaultTo sessionInitGhcOptions . many . strOption $ mconcat [
            long "ghc-option"
          , metavar "OPT"
          , helpDoc . Just $ Doc.vcat [
                "GHC option to use when initializing the session."
              , "Can be used multiple times; overrides default if used."
              ]
          ])
    <*> (defaultTo sessionInitRelativeIncludes . fmap splitSearchPath' . strOption $ mconcat [
            long "include"
          , metavar "PATH"
          , value ""
          , help "Include path (relative to session directory)."
          ])
    <*> useDefault sessionInitTargets
    <*> (defaultTo sessionInitRtsOpts . many . strOption $ mconcat [
            long "rts-option"
          , metavar "OPT"
          , helpDoc . Just $ Doc.vcat [
                "RTS option to use when initializing the session."
              , "Can be used multiple times; overrides default if used."
              ]
          ])
    <*> (defaultTo sessionInitDistDir . optional . strOption $ mconcat [
            long "dist-dir"
          , metavar "DIR"
          , helpDoc . Just $ Doc.vcat [
                "Directory for build artifacts. Defaults to: dist/"
              ]
          ])
  where
    useDefault :: (SessionInitParams -> a) -> Parser a
    useDefault fld = pure (fld defaultSessionInitParams)

    defaultTo :: (Alternative f) => (SessionInitParams -> f a) -> Parser (f a) -> Parser (f a)
    defaultTo fld = fmap $ \xs ->
      xs <|> fld defaultSessionInitParams

parseConfig :: Parser SessionConfig
parseConfig = mkSessionConfig
    <$> (strOption $ mconcat [
            long "dir"
          , metavar "DIR"
          , value (configDir defaultSessionConfig)
          , showDefault
          , help "Directory to use to store the session files"
          ])
    <*> (optional . strOption $ mconcat [
            long "local-work-dir"
          , metavar "DIR"
          , helpDoc . Just $ Doc.vcat [
                "Directory to find the source and data files."
              , "If this is passed, then there's no need for the editor to send the source / data files to the backend.  Instead, it just directly uses the files."
              ]
          ])
    <*> (fmap splitSearchPath' . strOption $ mconcat [
            long "ide-backend-tools-path"
          , metavar "PATH"
          , value ""
          , help "Path to search for suitable binaries of ide-backend-server and ide-backend-exe-cabal. Defaults to system path."
          ])
    <*> (defaultTo configExtraPathDirs . fmap splitSearchPath' . strOption $ mconcat [
            long "extra-path"
          , metavar "PATH"
          , value ""
          , helpDoc . Just $ Doc.vcat [
                "Additional search path to add to search for every tools (ghc, ide-backend-server, etc.)."
              , "Useful for overriding default locations."
              ]
          ])
    <*> (fmap mkPackageDBStack . strOption $ mconcat [
            long "package-db"
          , metavar "DIR"
          , value ""
          , helpDoc . Just $ Doc.vcat [
                "Package DBs to use for this session."
              , autoWrap "This takes the form of a search path; the resulting DB stack will consist of the global DB and the specified specific DBs."
              ]
          ])
    <*> (fmap not . switch $ mconcat [
            long "keep-tmp-files"
          , help "Do not delete the session directory on termination"
          ])
  where
    defaultTo :: (SessionConfig -> [a]) -> Parser [a] -> Parser [a]
    defaultTo fld = fmap $ \xs ->
      case xs of
        [] -> fld defaultSessionConfig
        _  -> xs

    mkPackageDBStack :: String -> PackageDBStack
    mkPackageDBStack ""   = configPackageDBStack defaultSessionConfig
    mkPackageDBStack path = GlobalPackageDB
                          : map SpecificPackageDB (splitSearchPath' path)

    ide_backend_server    = snd $ configIdeBackendServer   defaultSessionConfig
    ide_backend_exe_cabal = snd $ configIdeBackendExeCabal defaultSessionConfig

    mkSessionConfig configDir configLocalWorkingDir toolsPath configExtraPathDirs
                    configPackageDBStack configDeleteTempFiles
      = let
          SessionConfig{configInProcess,configGenerateModInfo,configLog,
                        configLicenseExc,configLicenseFixed}
            = defaultSessionConfig

          toolsPath' = (map ProgramSearchPathDir configExtraPathDirs) ++ case toolsPath of
            [] -> [ProgramSearchPathDefault]
            _  -> map ProgramSearchPathDir toolsPath
              -- NB. These binaries are tied to the version of ghc being used for the project;
              -- so if we are given a specific toolsPath but ide-backend-server is not found there,
              -- it is arguably better to just fail, and not use a potentially incorrect version of
              -- the binary that could be accidentally installed somewhere on the system path...
              -- (e.g. a 'file not found' error is simpler to diagnose for a user than an obscure
              -- 'invalid package db format' error)

          configIdeBackendServer   = (toolsPath', ide_backend_server)
          configIdeBackendExeCabal = (toolsPath', ide_backend_exe_cabal)

        in SessionConfig{..}


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

instance Show (a -> b) where
  show _ = "<<function>>"

-- | Version of splitSearchPath that returns the empty list on empty string
-- and expands home directories.
splitSearchPath' :: String -> [FilePath]
splitSearchPath' = map expandHomeDir . split
  where
    split "" = []
    split xs = splitSearchPath xs

-- | Expand home directory
expandHomeDir :: FilePath -> FilePath
expandHomeDir path = unsafePerformIO $ do
    home <- getHomeDirectory

    let expand :: FilePath -> FilePath
        expand []       = []
        expand ('~':xs) = home ++ expand xs
        expand (x:xs)   = x     : expand xs

    return $ expand path

-- | Insert automatic linebreaks
autoWrap :: String -> Doc.Doc
autoWrap = Chunk.extractChunk . Chunk.paragraph
