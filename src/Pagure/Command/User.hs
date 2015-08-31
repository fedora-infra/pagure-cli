module Pagure.Command.User where

import Control.Lens
import qualified Data.Text as T
import Web.Pagure

data UserOptions =
  UserOptions { userOptionsUsername :: String } deriving (Eq, Ord, Show)

userCommandRunner :: UserOptions -> IO ()
userCommandRunner (UserOptions username) = do
  let pc = PagureConfig "https://pagure.io" Nothing
  pagureUser <- runPagureT (user (T.pack username)) pc
  -- TODO: This output is very ugly
  putStrLn (show pagureUser)
