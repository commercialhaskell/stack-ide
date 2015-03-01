module Main where

import IdeSession.Client.JsonAPI
import IdeSession.Client.CmdLine

main :: IO ()
main = do
  Options{..} <- getCommandLineOptions
  case optCommand of
    ShowAPI ->
      putStrLn apiDocs
