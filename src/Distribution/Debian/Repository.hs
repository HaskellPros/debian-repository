{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Distribution.Debian.Repository
  ( Repository (..)
  , loadRepository
  ) where

import Control.Exception
import Control.Exception.Enclosed
import Control.Monad.Catch
import Data.Conduit
import Data.Monoid
import Data.Typeable
import Distribution.Debian.Repository.Backend
import Distribution.Debian.Repository.Conduit
import Distribution.Debian.Repository.PackagesIndex
import Distribution.Debian.Repository.ReleaseFile
import qualified Data.Text as T

data Repository = Repository
  { repositoryRoot           :: T.Text
  , repositoryReleaseFile    :: ReleaseFile
  , repositoryPackageIndices :: [PackagesIndex]
  } deriving (Show)

data RepositoryLoadException
  = RepositoryLoadBackendFailure SomeException
  | RepositoryLoadOtherFailure T.Text
  deriving (Show, Typeable)

instance Exception RepositoryLoadException

loadRepository :: T.Text -> T.Text -> [T.Text] -> IO Repository
loadRepository uri dist components = withUriSchemaBackend uri $ \backend -> do
  let releaseUri = uri <> "/dists/" <> dist <> "/Release"
  eitherReleaseFile <- tryAny $ repositoryUriRead backend releaseUri $ \rsrc -> rsrc $$+- releaseFileSink
  releaseFile <- case eitherReleaseFile of
    Left e -> throwM $ RepositoryLoadBackendFailure e
    Right Nothing -> throwM $ RepositoryLoadOtherFailure "No Release file emitted by parser"
    Right (Just x) -> return x
  return Repository
    { repositoryRoot = uri
    , repositoryReleaseFile = releaseFile
    , repositoryPackageIndices = []
    }
