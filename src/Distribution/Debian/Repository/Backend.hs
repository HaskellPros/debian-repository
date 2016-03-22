{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes #-}
module Distribution.Debian.Repository.Backend
  ( RepositoryUriSchema (..)
  , RepositoryUriSchemaBackend (..)
  , withUriSchemaBackend
  ) where

import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Data.ByteString as B
import qualified Data.Text as T

data RepositoryUriSchema
  = RepositoryUriSchemaHTTP
  | RepositoryUriSchemaUnknown

data RepositoryUriSchemaBackend m = RepositoryUriSchemaBackend
  { repositoryUriRead :: forall a . T.Text -> (ResumableSource m B.ByteString -> m a) -> m a
  }

detectUriSchema :: T.Text -> RepositoryUriSchema
detectUriSchema uri
  | "http://" `T.isPrefixOf` uri = RepositoryUriSchemaHTTP
  | otherwise = RepositoryUriSchemaUnknown

withSchemaBackend :: (MonadIO m, MonadBase IO m, MonadThrow m) => RepositoryUriSchema -> (RepositoryUriSchemaBackend (ResourceT m) -> m a) -> m a
withSchemaBackend s act = case s of
  RepositoryUriSchemaHTTP -> do
    manager <- liftIO $ newManager tlsManagerSettings
    act RepositoryUriSchemaBackend
      { repositoryUriRead = \urlText act -> do
          url <- parseUrl $ T.unpack urlText
          response <- http url manager
          if statusIsSuccessful $ responseStatus response
            then act $ responseBody response
            else throwM $ StatusCodeException (responseStatus response) (responseHeaders response) (responseCookieJar response)
      }

withUriSchemaBackend :: (MonadIO m, MonadBase IO m, MonadThrow m) => T.Text -> (RepositoryUriSchemaBackend (ResourceT m) -> m a) -> m a
withUriSchemaBackend uri = withSchemaBackend (detectUriSchema uri)
