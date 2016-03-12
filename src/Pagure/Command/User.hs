module Pagure.Command.User where

import Control.Lens
import qualified Data.Text as T
import Web.Pagure
import Web.Pagure.Lens

data UserOptions =
  UserOptions { userOptionsUsername :: String } deriving (Eq, Ord, Show)

userCommandRunner :: UserOptions -> IO ()
userCommandRunner (UserOptions username) = do
  let pc = PagureConfig "https://pagure.io" Nothing
  pagureUser <- runPagureT (userInfo (T.pack username)) pc
  case pagureUser of
    Nothing -> error $ "Unable to retrieve information for `" ++ username ++ "`"
    Just userRes -> do
      putStrLn $ "User      : " ++ userRes ^. user . name
      putStrLn $ "Full Name : " ++ userRes ^. user . fullname
      let repos' = userRes ^. repos
          forks' = userRes ^. forks
      putStrLn $ "# Repos   : " ++ show (length repos')
      putStrLn $ "# Forks   : " ++ show (length forks')
