{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Paths_pagure_cli as Paths
import Control.Lens hiding (argument)
import Data.Aeson.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import Options.Applicative
import Web.Pagure

-- | Options available to all @pg@ commands.
data GlobalOptions =
  GlobalOptions {
    globalOptionsCommand :: Command
  , globalOptionsVerbose :: Bool
  } deriving (Eq, Ord, Show)

data Command =
    Version
  | Tags TagsOptions
  deriving (Eq, Ord, Show)

data TagsOptions =
  TagsOptions { tagsOptionsRepo :: String } deriving (Eq, Ord, Show)

globalOptions :: Parser Command -> Parser GlobalOptions
globalOptions cmd = GlobalOptions
                    <$> cmd
                    <*> switch ( long "verbose"
                                 <> help "Output debugging information" )

versionCommand :: Parser Command
versionCommand = pure Version

tagsCommand :: Parser Command
tagsCommand = Tags <$> (TagsOptions <$> argument str (
                            metavar "REPOSITORY"
                         <> help "Repository to query for tags"))

options :: Parser GlobalOptions
options = subparser $
          command "version" (info (helper <*> globalOptions versionCommand) $
                             fullDesc
                          <>  progDesc "Displays the version of pagure-cli and the pagure.io API" )
       <> command "tags" (info (helper <*> globalOptions tagsCommand) $
                             fullDesc
                          <>  progDesc "Displays the tags for the given repository" )

runPagureCli :: GlobalOptions -> IO ()
runPagureCli g@(GlobalOptions cmd verbose) = case cmd of
  Version -> do
    -- TODO: Handle PagureConfig coming from CLI args and/or config file
    let pc = PagureConfig "https://pagure.io" Nothing
    pagureVersion <- runPagureT version pc
    putStrLn ("pagure-cli " ++ showVersion Paths.version)
    T.putStrLn ("pagure API "
                <> (fromMaybe "N/A" (pagureVersion ^? key "version" . _String))
                <> " (" <> "https://pagure.io" <> ")")
  Tags (TagsOptions repo) -> do
    let pc = PagureConfig "https://pagure.io" Nothing
    pagureTags <- runPagureT (tags repo) pc
    mapM_ T.putStrLn pagureTags

main :: IO ()
main = execParser opts >>= runPagureCli
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "pagure.io command-line interface"
     <> header "pg - interact with pagure.io from the command-line" )
