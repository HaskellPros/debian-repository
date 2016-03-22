{-# LANGUAGE OverloadedStrings #-}
module Distribution.Debian.Repository
  ( Repository (..)
  ) where

import Data.Monoid
import Distribution.Debian.Repository.Backend
import qualified Data.Text as T

data Repository = Repository
  { repositoryRoot :: T.Text
  }

loadRepository :: T.Text -> T.Text -> [T.Text] -> IO Repository
loadRepository uri dist components = withUriSchemaBackend uri $ \backend -> do
  let releaseUri = uri <> "/dists/" <> dist <> "/Release"
  undefined
