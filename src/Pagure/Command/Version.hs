{-# LANGUAGE OverloadedStrings #-}
module Pagure.Command.Version where

import Control.Lens
import Data.Aeson.Lens
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Version (showVersion)
import qualified Paths_pagure_cli as Paths
import qualified Data.Text.IO as T
import Web.Pagure

versionCommandRunner :: IO ()
versionCommandRunner = do
  -- TODO: Handle PagureConfig coming from CLI args and/or config file
  let pc = PagureConfig "https://pagure.io" Nothing
  pagureVersion <- runPagureT version pc
  putStrLn ("pagure-cli " ++ showVersion Paths.version)
  T.putStrLn ("pagure API "
              <> (fromMaybe "N/A" (pagureVersion ^? key "version" . _String))
              <> " (" <> "https://pagure.io" <> ")")
