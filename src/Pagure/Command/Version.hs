{-# LANGUAGE OverloadedStrings #-}
module Pagure.Command.Version where

import Control.Lens
import Data.Aeson.Lens
import Data.Default
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Version (showVersion)
import Pagure.Utility.Git
import qualified Paths_pagure_cli as Paths
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Web.Pagure
import Web.Pagure.Lens

versionCommandRunner :: IO ()
versionCommandRunner = do
  pc' <- pagureConfigWithAuth
  -- TODO: Don't use def here.
  let pc = fromMaybe def pc'
  pagureVersion <- runPagureT version pc
  putStrLn ("pagure-cli " ++ showVersion Paths.version)
  T.putStrLn ("pagure API "
              <> (fromMaybe "N/A" (pagureVersion ^? key "version" . _String))
              <> " (" <> T.pack (_baseUrl pc) <> ")")
  putStrLn ("Authenticating with key: " ++ show (_apiKey pc))
