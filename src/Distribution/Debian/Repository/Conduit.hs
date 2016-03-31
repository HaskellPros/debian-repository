{-# LANGUAGE DeriveDataTypeable #-}
module Distribution.Debian.Repository.Conduit
  ( IncrementalParseException (..)
  , incrementalParserSink
  , packagesIndexSink
  , packagesIndexSource
  , releaseFileSink
  , releaseFileSource
  ) where

import Control.Exception
import Control.Monad.Catch
import Data.Conduit
import Data.Typeable
import Distribution.Debian.Repository.Parse
import Distribution.Debian.Repository.PackagesIndex
import Distribution.Debian.Repository.ReleaseFile
import qualified Data.ByteString as B
import qualified Data.Text as T

newtype IncrementalParseException = IncrementalParseException T.Text
  deriving (Eq, Show, Typeable)

instance Exception IncrementalParseException

incrementalParserSink :: (Monad m, MonadThrow m) => IncrementalParser i r -> i -> Sink i m (Maybe r)
incrementalParserSink parser emptyInput = loop parser
  where
    loop parser = await >>= \maybeInput -> case maybeInput of
      Just input -> case parser input of
        IncrementalParseOk _ nextParser -> loop nextParser
        IncrementalParseFail e -> throwM $ IncrementalParseException e
      Nothing -> case parser emptyInput of
        IncrementalParseOk result _ -> return result
        IncrementalParseFail e -> throwM $ IncrementalParseException e

packagesIndexSink :: (Monad m, MonadThrow m) => Sink B.ByteString m (Maybe PackagesIndex)
packagesIndexSink = incrementalParserSink parsePackages B.empty

releaseFileSink :: (Monad m, MonadThrow m) => Sink B.ByteString m (Maybe ReleaseFile)
releaseFileSink = incrementalParserSink parseReleaseFile B.empty

packagesIndexSource :: Monad m => PackagesIndex -> Source m B.ByteString
packagesIndexSource packagesIndex = storePackages packagesIndex yield

releaseFileSource :: Monad m => ReleaseFile -> Source m B.ByteString
releaseFileSource releaseFile = storeReleaseFile releaseFile yield
