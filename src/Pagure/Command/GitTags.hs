module Pagure.Command.GitTags where

import Control.Lens
import qualified Data.Text.IO as T
import Web.Pagure

data GitTagsOptions =
  GitTagsOptions { gitTagsOptionsRepo :: String } deriving (Eq, Ord, Show)

gitTagsCommandRunner :: GitTagsOptions -> IO ()
gitTagsCommandRunner (GitTagsOptions repo) = do
  -- TODO: Handle PagureConfig coming from CLI args and/or config file
  let pc = PagureConfig "https://pagure.io" Nothing
  gitTagsResp <- runPagureT (gitTags repo) pc
  mapM_ T.putStrLn gitTagsResp
