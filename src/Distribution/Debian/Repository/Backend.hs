{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes #-}
module Distribution.Debian.Repository.Backend
  ( RepositoryUriSchema (..)
  , RepositoryUriSchemaBackend (..)
  , RepositoryReadError (..)
  , withUriSchemaBackend
  ) where

import Control.Exception (SomeException)
import Control.Monad.Catch
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

data RepositoryReadError
  = RepositoryReadNotFound
  | RepositoryReadOther SomeException

data RepositoryUriSchemaBackend m = RepositoryUriSchemaBackend
  { repositoryUriRead :: forall a . T.Text -> (ResumableSource m B.ByteString -> m a) -> m (Either RepositoryReadError a)
  }

detectUriSchema :: T.Text -> RepositoryUriSchema
detectUriSchema uri
  | "http://" `T.isPrefixOf` uri = RepositoryUriSchemaHTTP
  | otherwise = RepositoryUriSchemaUnknown

withSchemaBackend :: (MonadIO m, MonadBaseControl IO m, MonadCatch m, MonadThrow m) => RepositoryUriSchema -> (RepositoryUriSchemaBackend (ResourceT m) -> (ResourceT m) a) -> m a
withSchemaBackend s act = case s of
  RepositoryUriSchemaHTTP -> do
    manager <- liftIO $ newManager tlsManagerSettings
    runResourceT $ act RepositoryUriSchemaBackend
      { repositoryUriRead = \urlText act -> do
          url <- parseUrl $ T.unpack urlText
          (http url manager >>= (\x -> Right <$> act (responseBody x))) `catch`
            (\e@(StatusCodeException s _ _) ->
               return $ if statusCode s == 404
                 then Left RepositoryReadNotFound
                 else Left . RepositoryReadOther $ SomeException e)
      }

withUriSchemaBackend :: (MonadIO m, MonadBaseControl IO m, MonadCatch m, MonadThrow m) => T.Text -> (RepositoryUriSchemaBackend (ResourceT m) -> (ResourceT m) a) -> m a
withUriSchemaBackend uri = withSchemaBackend (detectUriSchema uri)
