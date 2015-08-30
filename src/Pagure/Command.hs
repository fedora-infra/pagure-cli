{-# LANGUAGE OverloadedStrings #-}

module Pagure.Command where

import qualified Paths_pagure_cli as Paths
import Control.Lens hiding (argument)
import Data.Aeson.Lens
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import Options.Applicative
import Web.Pagure hiding (User)

-- | Every command has a constructor in this type. It can have an optional field
-- which should contain the command-line options specific to that command.
data Command =
    GitTags GitTagsOptions
  | Tags TagsOptions
  | User UserOptions
  | Users UsersOptions
  | Version
  deriving (Eq, Ord, Show)

-- | Every command should also have its relevant @command@ (defined below)
-- referenced here.
options :: Parser GlobalOptions
options = subparser $
          gitTagsCommand
       <> tagsCommand
       <> userCommand
       <> usersCommand
       <> versionCommand


-- | Every command should also have a relevant function that actually does what
-- the command intends to do. That function should be referenced here. This
-- function should always be total vis-a-vis 'Command' above.
runPagureCli :: GlobalOptions -> IO ()
runPagureCli g@(GlobalOptions cmd verbose) =
  case cmd of
    GitTags opts -> gitTagsCommandRunner opts
    Tags opts    -> tagsCommandRunner opts
    User opts    -> userCommandRunner opts
    Users opts   -> usersCommandRunner opts
    Version      -> versionCommandRunner


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
                    <*> switch (long "verbose"
                             <> help "Output debugging information")


--------------------------------------------------------------------------------
-- Command: git-tags
--------------------------------------------------------------------------------

data GitTagsOptions =
  GitTagsOptions { gitTagsOptionsRepo :: String } deriving (Eq, Ord, Show)

gitTagsCommandParser :: Parser Command
gitTagsCommandParser = GitTags <$> (GitTagsOptions <$> argument str (
                                                         metavar "REPOSITORY"
                                                      <> help "Repository to query for tags"))

gitTagsCommand :: Mod CommandFields GlobalOptions
gitTagsCommand =
  command "git-tags" (info (helper <*> globalOptions gitTagsCommandParser) $
                      fullDesc
                      <> progDesc "Displays the git tags for the given repository")

gitTagsCommandRunner :: GitTagsOptions -> IO ()
gitTagsCommandRunner (GitTagsOptions repo) = do
  -- TODO: Handle PagureConfig coming from CLI args and/or config file
  let pc = PagureConfig "https://pagure.io" Nothing
  gitTagsResp <- runPagureT (gitTags repo) pc
  mapM_ T.putStrLn gitTagsResp


--------------------------------------------------------------------------------
-- Command: tags
--------------------------------------------------------------------------------

data TagsOptions =
  TagsOptions { tagsOptionsRepo :: String
              , tagsOptionsPattern :: Maybe String } deriving (Eq, Ord, Show)

tagsCommandParser :: Parser Command
tagsCommandParser = Tags <$> (TagsOptions <$> argument str (
                                                metavar "REPOSITORY"
                                             <> help "Repository to query for tags")
                                          <*> optional (strOption (
                                                long "pattern"
                                             <> short 'p'
                                             <> metavar "PATTERN"
                                             <> help "An optional beginning pattern to filter by")))

tagsCommand :: Mod CommandFields GlobalOptions
tagsCommand =
  command "tags" (info (helper <*> globalOptions tagsCommandParser) $
                  fullDesc
                  <>  progDesc "Displays the tags for the given repository")

tagsCommandRunner :: TagsOptions -> IO ()
tagsCommandRunner (TagsOptions repo pattern) = do
  let pc = PagureConfig "https://pagure.io" Nothing
  pagureTags <- runPagureT (tags repo (fmap T.pack pattern)) pc
  mapM_ T.putStrLn pagureTags


--------------------------------------------------------------------------------
-- Command: user
--------------------------------------------------------------------------------

data UserOptions =
  UserOptions { userOptionsUsername :: String } deriving (Eq, Ord, Show)

userCommandParser :: Parser Command
userCommandParser = User <$> (UserOptions <$> argument str (
                                                metavar "USERNAME"
                                             <> help "The username to look up"))

userCommand :: Mod CommandFields GlobalOptions
userCommand =
  command "user" (info (helper <*> globalOptions userCommandParser) $
                   fullDesc
                   <>  progDesc "Show information about a pagure user")

userCommandRunner :: UserOptions -> IO ()
userCommandRunner (UserOptions username) = do
  let pc = PagureConfig "https://pagure.io" Nothing
  pagureUser <- runPagureT (user (T.pack username)) pc
  -- TODO: This output is very ugly
  putStrLn (show pagureUser)


--------------------------------------------------------------------------------
-- Command: users
--------------------------------------------------------------------------------

data UsersOptions =
  UsersOptions { usersOptionsPattern :: Maybe String } deriving (Eq, Ord, Show)

usersCommandParser :: Parser Command
usersCommandParser = Users <$> (UsersOptions <$> optional (strOption (
                                                   long "pattern"
                                                <> short 'p'
                                                <> metavar "PATTERN"
                                                <> help "An optional beginning pattern to filter by")))

usersCommand :: Mod CommandFields GlobalOptions
usersCommand =
  command "users" (info (helper <*> globalOptions usersCommandParser) $
                   fullDesc
                   <>  progDesc "Retieve a list of pagure users")

usersCommandRunner :: UsersOptions -> IO ()
usersCommandRunner (UsersOptions pattern) = do
  let pc = PagureConfig "https://pagure.io" Nothing
  pagureUsers <- runPagureT (users (fmap T.pack pattern)) pc
  mapM_ T.putStrLn pagureUsers


--------------------------------------------------------------------------------
-- Command: version
--------------------------------------------------------------------------------

versionCommandParser :: Parser Command
versionCommandParser = pure Version

versionCommand :: Mod CommandFields GlobalOptions
versionCommand =
  command "version" (info (helper <*> globalOptions versionCommandParser) $
                     fullDesc
                     <>  progDesc "Displays the version of pagure-cli and the pagure.io API")

versionCommandRunner :: IO ()
versionCommandRunner = do
  -- TODO: Handle PagureConfig coming from CLI args and/or config file
  let pc = PagureConfig "https://pagure.io" Nothing
  pagureVersion <- runPagureT version pc
  putStrLn ("pagure-cli " ++ showVersion Paths.version)
  T.putStrLn ("pagure API "
              <> (fromMaybe "N/A" (pagureVersion ^? key "version" . _String))
              <> " (" <> "https://pagure.io" <> ")")
