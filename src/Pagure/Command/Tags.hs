module Pagure.Command.Tags where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Web.Pagure

data TagsOptions =
  TagsOptions { tagsOptionsRepo :: String
              , tagsOptionsPattern :: Maybe String } deriving (Eq, Ord, Show)

tagsCommandRunner :: TagsOptions -> IO ()
tagsCommandRunner (TagsOptions repo pattern) = do
  let pc = PagureConfig "https://pagure.io" Nothing
  pagureTags <- runPagureT (tags repo (fmap T.pack pattern)) pc
  mapM_ T.putStrLn pagureTags
