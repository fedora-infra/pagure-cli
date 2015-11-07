{-# LANGUAGE OverloadedStrings #-}
module Pagure.Utility.Git where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Monoid
import qualified Data.Text as T
import Web.Pagure
import qualified Shelly as S

getPagureGitConfig :: T.Text -> MaybeT S.Sh String
getPagureGitConfig key = do
  res <- S.shelly .
         S.silently .
         S.errExit False $
         S.run "git" ["config", "pagure." <> key]
  ec <- lift $ S.lastExitCode
  guard (ec == 0 && not (T.null res))
  return (init . T.unpack $ res)

pagureConfigFromGit :: MaybeT S.Sh PagureConfig
pagureConfigFromGit = do
  pagureInstance <- getPagureGitConfig "instance"
  key <- getPagureGitConfig "key"
  return $ PagureConfig pagureInstance (Just key)

pagureConfigWithAuth :: IO (Maybe PagureConfig)
pagureConfigWithAuth = S.shelly . S.silently $ runMaybeT pagureConfigFromGit
