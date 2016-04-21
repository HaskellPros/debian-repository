{-# LANGUAGE FlexibleContexts, OverloadedStrings, ScopedTypeVariables, RankNTypes #-}
module Distribution.Debian.Repository.Backend
  ( RepositoryUriSchema (..)
  , RepositoryUriSchemaBackend (..)
  , RepositoryReadError (..)
  , detectUriSchema
  , withUriSchemaBackend
  ) where

import Control.Exception (SomeException)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Binary
import Network.HTTP.Conduit
import Network.HTTP.Types
import System.IO
import System.IO.Error
import qualified Data.ByteString as B
import qualified Data.Text as T

data RepositoryUriSchema
  = RepositoryUriSchemaHTTP
  | RepositoryUriSchemaFile
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
  | "file://" `T.isPrefixOf` uri = RepositoryUriSchemaFile
  | otherwise = RepositoryUriSchemaUnknown

withSchemaBackend :: forall a m . (MonadIO m, MonadBaseControl IO m, MonadCatch m, MonadThrow m) => RepositoryUriSchema -> (RepositoryUriSchemaBackend (ResourceT m) -> (ResourceT m) a) -> m a
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
  RepositoryUriSchemaFile -> runResourceT $ act RepositoryUriSchemaBackend
    { repositoryUriRead = fileUriRead
    }
  where
    -- This one was an extremely tricky beast to typecheck
    fileUriRead :: forall b . T.Text -> (ResumableSource (ResourceT m) B.ByteString -> ResourceT m b) -> ResourceT m (Either RepositoryReadError b)
    fileUriRead uriText act = control
      (\runInBase -> withFile (T.unpack $ stripFilePrefix uriText) ReadMode $ \handle ->
        runInBase (Right <$> (act . newResumableSource $ sourceHandle handle) :: ResourceT m (Either RepositoryReadError b))
      ) `catch`
      (\(e :: IOError) -> return $
        if isDoesNotExistError e then Left RepositoryReadNotFound else Left . RepositoryReadOther $ SomeException e
      ) :: ResourceT m (Either RepositoryReadError b)

stripFilePrefix :: T.Text -> T.Text
stripFilePrefix t = if "file://" `T.isPrefixOf` t
  then T.drop (T.length "file://") t
  else t

withUriSchemaBackend :: (MonadIO m, MonadBaseControl IO m, MonadCatch m, MonadThrow m) => T.Text -> (RepositoryUriSchemaBackend (ResourceT m) -> (ResourceT m) a) -> m a
withUriSchemaBackend uri = withSchemaBackend (detectUriSchema uri)
