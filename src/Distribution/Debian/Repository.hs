{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings, TemplateHaskell #-}
module Distribution.Debian.Repository
  ( Repository (..)
  , LoadRepositoryArgs (..)
  , loadRepository
  ) where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Primitive
import Control.Monad.Trans.Control
import Data.Attoparsec.Text
import Data.Conduit
import Data.Conduit.Zlib (ungzip)
import Data.List (groupBy)
import Data.Monoid
import Data.Typeable
import Prelude hiding (takeWhile)
import Distribution.Debian.Repository.Backend
import Distribution.Debian.Repository.Conduit
import Distribution.Debian.Repository.PackagesIndex
import Distribution.Debian.Repository.ReleaseFile
import qualified Data.Text as T

logSource :: T.Text
logSource = "Distribution.Debian.Repository"

data Repository = Repository
  { repositoryRoot           :: T.Text
  , repositoryReleaseFile    :: ReleaseFile
  , repositoryPackageIndices :: [PackagesIndex]
  } deriving (Eq, Show)

data RepositoryLoadException
  = RepositoryLoadBackendFailure SomeException
  | RepositoryLoadOtherFailure T.Text
  deriving (Show, Typeable)

instance Exception RepositoryLoadException

data LoadRepositoryArgs = LoadRepositoryArgs
  { loadRepositoryArchitecture :: T.Text
  -- ^ Target architecture to load packages info
  , loadRepositoryUri :: T.Text
  -- ^ URI of the repository to be loaded
  , loadRepositoryDist :: T.Text
  -- ^ Distribution codename to be loaded
  , loadRepositoryComponents :: [T.Text]
  -- ^ Components to load
  }

loadRepository
  :: (MonadBaseControl IO m, MonadIO m, MonadLogger m, MonadCatch m, MonadThrow m)
  => LoadRepositoryArgs -> m Repository
loadRepository (LoadRepositoryArgs arch uri dist components) = withUriSchemaBackend uri $ \backend -> do
  let distUri = uri <> "/dists/" <> dist
      releaseUri = distUri <> "/Release"
  $logInfoS logSource $ "Loading Release file from " <> releaseUri
  eitherReleaseFile <- repositoryUriRead backend releaseUri $ \rsrc -> rsrc $$+- releaseFileSink
  releaseFile <- case eitherReleaseFile of
    Left RepositoryReadNotFound -> throwM $ RepositoryLoadOtherFailure "Release file not found at given URI"
    Left (RepositoryReadOther e) -> throwM $ RepositoryLoadBackendFailure e
    Right Nothing -> throwM $ RepositoryLoadOtherFailure "No Release file emitted by parser"
    Right (Just x) -> return x
  $logInfoS logSource "Release file loaded and parsed successfully. Evaluating list of package indices to be downloaded."
  let packageEntries = concatMap
        (\comp ->
              findComponentPackages releaseFile comp arch
           ++ findComponentPackages releaseFile comp "all"
        ) components
      packageFileInfos = toPackagesFileInfos packageEntries
  $logInfoS logSource $ "The following Packages files are to be downloaded: " <> T.pack (show packageFileInfos)
  packageIndices <- forM packageFileInfos $ loadPackagesFile backend distUri
  return Repository
    { repositoryRoot = uri
    , repositoryReleaseFile = releaseFile
    , repositoryPackageIndices = packageIndices
    }

loadPackagesFile
  :: (MonadBase base m, PrimMonad base, MonadLogger m, MonadCatch m, MonadThrow m)
  => RepositoryUriSchemaBackend m -> T.Text -> PackagesFileInfo -> m PackagesIndex
loadPackagesFile backend rootUri pfi = do
    mr <- foldM (tryLoadPackage $ packagesFileBaseDir pfi) Nothing (packagesFileAltFilenames pfi)
    case mr of
      Just x -> return x
      Nothing -> throwM . RepositoryLoadOtherFailure $ "Failed to load packages index from base dir \"" <> packagesFileBaseDir pfi <> "\""
  where
    tryLoadPackage baseDir alreadyLoaded altName =
      let filename = rootUri <> "/" <> baseDir <> "/" <> altName
          loadWith snk = do
              loadPackagesResult <- repositoryUriRead backend filename $ \rsrc -> rsrc $$+- snk
              case loadPackagesResult of
                Left RepositoryReadNotFound -> do
                  $logWarnS logSource $ "Failed to load package from \"" <> filename <> "\" - file not found. Going on with other alt names."
                  return Nothing
                Left (RepositoryReadOther e) -> do
                  $logWarnS logSource $ "Failed to load package from \"" <> filename <> "\" with exception \"" <> T.pack (show e) <> "\". Going on with other alt names."
                  return Nothing
                Right Nothing -> do
                  $logWarnS logSource $ "Failed to load package from \"" <> filename <> "\" with parser failing to yield value. Going on with other alt names."
                  return Nothing
                Right (Just x) -> return (Just x)
      in case alreadyLoaded of
          Just x -> return (Just x)
          Nothing -> case getExtension altName of
            "" -> loadWith packagesIndexSink
            ".gz" -> loadWith (ungzip =$= packagesIndexSink)
            _ -> do
              $logWarnS logSource $ "Failed to load package from \"" <> filename <> "\" because its extension is not supported. Going on with other alt names."
              return Nothing
    getExtension x = case T.splitOn "." x of
      [] -> ""
      [x] -> ""
      xs -> "." <> last xs

data PackagesFileInfo = PackagesFileInfo
  { packagesFileBaseDir      :: T.Text
  , packagesFileAltFilenames :: [T.Text]
  } deriving (Show)

toPackagesFileInfos :: [Md5SumEntry] -> [PackagesFileInfo]
toPackagesFileInfos md5sums =
    let dirsWithFiles = map (splitDir . md5SumEntryFilename) md5sums
        dirGroups = groupBy (\x y -> fst x == fst y) dirsWithFiles
    in  map groupToPackagesFileInfo dirGroups
  where
    splitDir x = case T.splitOn "/" x of
      [] -> ("", "")
      [x] -> ("", x)
      xs -> (T.intercalate "/" (init xs), last xs)
    groupToPackagesFileInfo g = PackagesFileInfo
      { packagesFileBaseDir = fst $ head g
      , packagesFileAltFilenames = map snd g
      }

-- | Searches for package files for given component and architecture in given Release file
findComponentPackages :: ReleaseFile -> T.Text -> T.Text -> [Md5SumEntry]
findComponentPackages releaseFile component arch =
    case releaseFile ^. at "MD5Sum" of
      Nothing -> []
      Just md5sum -> filter isSuitableEntry $ parseMd5Sum md5sum
  where
    isSuitableEntry x = case T.splitOn "/" (md5SumEntryFilename x) of
      [] -> False
      parts -> length parts > 2 && isSuitableBinary (last $ init parts) && "Packages" `T.isPrefixOf` last parts
    isSuitableBinary x = x == "binary-" <> arch

data Md5SumEntry = Md5SumEntry
  { md5SumEntryMd5      :: T.Text
  , md5SumEntrySize     :: Int
  , md5SumEntryFilename :: T.Text
  } deriving (Eq, Show)

parseMd5Sum :: T.Text -> [Md5SumEntry]
parseMd5Sum input = case parseOnly (md5Parser []) input of
    Left _ -> [] -- Probably it's a good place to put some error handling, but that's yet to be done.
    Right x -> reverse x
  where
    md5Parser current = do
      entry <- option Nothing (Just <$> parseLine)
      case entry of
        Nothing -> return current
        Just x -> md5Parser (x:current)
    parseLine = do
      skipSpace
      hash <- takeWhile (not .  isHorizontalSpace)
      skipSpace
      size <- decimal
      skipSpace
      filename <- takeWhile (\x -> not (isHorizontalSpace x) && not (isEndOfLine x))
      endOfLine
      return $ Md5SumEntry hash size filename
