module Pagure.Command.Users where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Web.Pagure

data UsersOptions =
  UsersOptions { usersOptionsPattern :: Maybe String } deriving (Eq, Ord, Show)

usersCommandRunner :: UsersOptions -> IO ()
usersCommandRunner (UsersOptions pattern) = do
  let pc = PagureConfig "https://pagure.io" Nothing
  pagureUsers <- runPagureT (users (fmap T.pack pattern)) pc
  mapM_ T.putStrLn pagureUsers
