{-# LANGUAGE OverloadedStrings #-}
module Pagure.Command.Issues where

import Control.Lens
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Maybe
import qualified Data.Text as T
import Pagure.Utility.Git
import Web.Pagure
import Web.Pagure.Issues -- TODO: Web.Pagure should export this.
import Web.Pagure.Lens as L
import qualified Shelly as S -- TODO: Make Pagure.Utility.Git export a nicer interface
import System.Console.ANSI
import Text.PrettyPrint.Boxes

data IssuesOptions =
  IssuesOptions { issuesOptionsRepo :: String
                , issuesOptionsStatus :: Maybe String
                , issuesOptionsTags :: Maybe String
                , issuesOptionsAssignee :: Maybe String
                , issuesOptionsAuthor :: Maybe String
                } deriving (Eq, Ord, Show)

issuesCommandRunner :: IssuesOptions -> IO ()
issuesCommandRunner (IssuesOptions repoName status tags assignee author) = do
  let pc = PagureConfig "https://pagure.io" Nothing
  --pagureReponame <- S.shelly . S.silently . runMaybeT $ getPagureGitConfig "repo"
  --case pagureReponame of
  --  Nothing -> error $ "Please set the git config key 'pagure.repo' and retry."
  --  Just repoName -> do
  repoIssues <- runPagureT (issues repoName def) pc
  printBox header
  printBox $ vcat left (map printIssue (repoIssues ^. issueList))

alignedRow :: String -> String -> String -> String -> Box
alignedRow a b c d =
  alignHoriz left 5 (text a) <+>
  alignHoriz left 60 (text b) <+>
  alignHoriz left 10 (text c) <+>
  alignHoriz left 20 (text d)

header :: Box
header =
  alignedRow
    (boldify "#")
    (boldify "Title")
    (boldify "Status")
    (boldify "Assignee")
  where
    boldify s = s
      -- TODO:
      -- setSGRCode [SetConsoleIntensity BoldIntensity] ++
      -- s ++
      -- setSGRCode [SetConsoleIntensity NormalIntensity]

printIssue :: Issue -> Box
printIssue i =
  alignedRow (i ^. L.id . to show)
             (i ^. title)
             (i ^. status)
             (fromMaybe "<unassigned>" (i ^? assignee . _Just . name))
