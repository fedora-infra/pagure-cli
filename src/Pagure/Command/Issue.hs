{-# LANGUAGE OverloadedStrings #-}
module Pagure.Command.Issue where

import Control.Lens
import Data.Default
import Data.Maybe
import qualified Data.Text as T
import Pagure.Utility.Git
import Web.Pagure
import Web.Pagure.Issues -- TODO: Web.Pagure should export this.
import Web.Pagure.Lens as L
import qualified Shelly as S -- TODO: Make Pagure.Utility.Git export a nicer interface

data IssueOptions =
  IssueOptions { issuesOptionsRepo :: String
               , issuesOptionsId :: Integer
               } deriving (Eq, Ord, Show)

issueCommandRunner :: IssueOptions -> IO ()
issueCommandRunner (IssueOptions repoName idnum) = do
  let pc = PagureConfig "https://pagure.io" Nothing
  issueInfo <- runPagureT (issue repoName idnum) pc
  prettyPrintIssue issueInfo

prettyPrintIssue :: Issue -> IO ()
prettyPrintIssue i = do
  putStrLn $ "Title          : " ++ i ^. title
  putStrLn $ "Status         : " ++ i ^. status
  putStrLn $ "Private        : " ++ i ^. private . to show
  putStrLn $ "Author         : " ++ i ^. user . name
  putStrLn $ "Assigned To    : " ++
    (fromMaybe "<unassigned>" (i ^? assignee . _Just . name))
  putStrLn $ "# Comments     : " ++ i ^. comments . to (show . length)
  putStrLn $ "# Dependencies : " ++ i ^. depends . to (show . length)
  putStrLn $ "# Blocking     : " ++ i ^. blocks . to (show . length)
  putStrLn $ "Tags           : " ++ i ^. tagList . to show
  putStrLn $ "Content        : " ++ i ^. content
