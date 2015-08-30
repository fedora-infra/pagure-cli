{-# LANGUAGE OverloadedStrings #-}

module Pagure.Command where

import qualified Paths_pagure_cli as Paths
import Control.Lens hiding (argument)
import Data.Aeson.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import Options.Applicative
import Web.Pagure

-- | Every command has a constructor in this type. It can have an optional field
-- which should contain the command-line options specific to that command.
data Command =
    Version
  | Tags TagsOptions
  deriving (Eq, Ord, Show)

-- | Every command should also have its relevant @command@ (defined below)
-- referenced here.
options :: Parser GlobalOptions
options = subparser $
          versionCommand
       <> tagsCommand

-- | Every command should also have a relevant function that actually does what
-- the command intends to do. That function should be referenced here. This
-- function should always be total vis-a-vis 'Command' above.
runPagureCli :: GlobalOptions -> IO ()
runPagureCli g@(GlobalOptions cmd verbose) =
  case cmd of
    Version -> versionCommandRunner
    Tags opts -> tagsCommandRunner opts


--------------------------------------------------------------------------------
-- Global options, available to all commands.
--------------------------------------------------------------------------------

-- | Options available to all @pg@ commands.
data GlobalOptions =
  GlobalOptions {
    globalOptionsCommand :: Command
  , globalOptionsVerbose :: Bool
  } deriving (Eq, Ord, Show)

globalOptions :: Parser Command -> Parser GlobalOptions
globalOptions cmd = GlobalOptions
                    <$> cmd
                    <*> switch ( long "verbose"
                                 <> help "Output debugging information" )


--------------------------------------------------------------------------------
-- Command: tags
--------------------------------------------------------------------------------

data TagsOptions =
  TagsOptions { tagsOptionsRepo :: String } deriving (Eq, Ord, Show)

tagsCommandParser :: Parser Command
tagsCommandParser = Tags <$> (TagsOptions <$> argument str (
                                  metavar "REPOSITORY"
                                  <> help "Repository to query for tags"))

tagsCommand :: Mod CommandFields GlobalOptions
tagsCommand =
  command "tags" (info (helper <*> globalOptions tagsCommandParser) $
                  fullDesc
                  <>  progDesc "Displays the tags for the given repository" )

tagsCommandRunner :: TagsOptions -> IO ()
tagsCommandRunner (TagsOptions repo) = do
  let pc = PagureConfig "https://pagure.io" Nothing
  pagureTags <- runPagureT (tags repo) pc
  mapM_ T.putStrLn pagureTags


--------------------------------------------------------------------------------
-- Command: version
--------------------------------------------------------------------------------

versionCommandParser :: Parser Command
versionCommandParser = pure Version

versionCommand :: Mod CommandFields GlobalOptions
versionCommand =
  command "version" (info (helper <*> globalOptions versionCommandParser) $
                     fullDesc
                     <>  progDesc "Displays the version of pagure-cli and the pagure.io API" )

versionCommandRunner :: IO ()
versionCommandRunner = do
  -- TODO: Handle PagureConfig coming from CLI args and/or config file
  let pc = PagureConfig "https://pagure.io" Nothing
  pagureVersion <- runPagureT version pc
  putStrLn ("pagure-cli " ++ showVersion Paths.version)
  T.putStrLn ("pagure API "
              <> (fromMaybe "N/A" (pagureVersion ^? key "version" . _String))
              <> " (" <> "https://pagure.io" <> ")")
