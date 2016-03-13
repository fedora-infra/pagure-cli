module Pagure.Command where

import Options.Applicative
import Web.Pagure hiding (User)

import Pagure.Command.GitTags
import Pagure.Command.Issues
import Pagure.Command.Tags
import Pagure.Command.User
import Pagure.Command.Users
import Pagure.Command.Version

-- | Every command has a constructor in this type. It can have an optional field
-- which should contain the command-line options specific to that command.
data Command =
    GitTags GitTagsOptions
  | Issues IssuesOptions
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
       <> issuesCommand
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
    Issues opts  -> issuesCommandRunner opts
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

gitTagsCommandParser :: Parser Command
gitTagsCommandParser = GitTags <$> (GitTagsOptions <$> argument str (
                                                         metavar "REPOSITORY"
                                                      <> help "Repository to query for tags"))

gitTagsCommand :: Mod CommandFields GlobalOptions
gitTagsCommand =
  command "git-tags" (info (helper <*> globalOptions gitTagsCommandParser) $
                      fullDesc
                   <> progDesc "Displays the git tags for the given repository")


--------------------------------------------------------------------------------
-- Command: issues
--------------------------------------------------------------------------------

issuesCommandParser :: Parser Command
issuesCommandParser = Issues <$> (IssuesOptions <$> argument str (
                                                      metavar "REPOSITORY"
                                                   <> help "Repository to query for issues")
                                                <*> optional (strOption (
                                                      long "status"
                                                   <> short 's'
                                                   <> metavar "STATUS"
                                                   <> help "Filter by issue status"))
                                                <*> optional (strOption (  -- TODO: List
                                                      long "tags"
                                                   <> short 't'
                                                   <> metavar "TAGS"
                                                   <> help "Filter by issue tags"))
                                                <*> optional (strOption (
                                                      long "assignee"
                                                   <> short 'A'
                                                   <> metavar "ASSIGNEE"
                                                   <> help "Filter by assignee"))
                                                <*> optional (strOption (
                                                      long "author"
                                                   <> short 'a'
                                                   <> metavar "AUTHOR"
                                                   <> help "Filter by author")))

issuesCommand :: Mod CommandFields GlobalOptions
issuesCommand =
  command "issues" (info (helper <*> globalOptions issuesCommandParser) $
                    fullDesc
                 <> progDesc "List issues for a given repository")


--------------------------------------------------------------------------------
-- Command: tags
--------------------------------------------------------------------------------

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
               <> progDesc "Displays the tags for the given repository")


--------------------------------------------------------------------------------
-- Command: user
--------------------------------------------------------------------------------

userCommandParser :: Parser Command
userCommandParser = User <$> (UserOptions <$> argument str (
                                                metavar "USERNAME"
                                             <> help "The username to look up"))

userCommand :: Mod CommandFields GlobalOptions
userCommand =
  command "user" (info (helper <*> globalOptions userCommandParser) $
                   fullDesc
                <> progDesc "Show information about a pagure user")


--------------------------------------------------------------------------------
-- Command: users
--------------------------------------------------------------------------------

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
                <> progDesc "Retieve a list of pagure users")


--------------------------------------------------------------------------------
-- Command: version
--------------------------------------------------------------------------------

versionCommandParser :: Parser Command
versionCommandParser = pure Version

versionCommand :: Mod CommandFields GlobalOptions
versionCommand =
  command "version" (info (helper <*> globalOptions versionCommandParser) $
                     fullDesc
                  <> progDesc "Displays the version of pagure-cli and the pagure.io API")
