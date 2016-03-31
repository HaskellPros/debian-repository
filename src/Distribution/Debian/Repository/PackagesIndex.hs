{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Distribution.Debian.Repository.PackagesIndex
  ( Package (..)
  , PackagesIndex (..)
  , IncrementalParseResult (..)
  , parsePackages
  , storePackages
  ) where

import Control.Applicative
import Control.Monad
import Control.Lens
import Control.Lens.TH
import Data.Attoparsec.Text
import Data.Map.Strict (Map)
import Data.Monoid
import Debug.Trace
import Distribution.Debian.Repository.Package
import Distribution.Debian.Repository.Parse
import Prelude hiding (takeWhile)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data PackagesIndex = PackagesIndex
  { _indexPackages :: Map T.Text Package
  } deriving (Eq, Show)

makeLenses ''PackagesIndex

type instance Index PackagesIndex = T.Text
type instance IxValue PackagesIndex = Package

instance Ixed PackagesIndex where
  ix field = indexPackages . ix field

instance At PackagesIndex where
  at field = indexPackages . at field

newtype ParsePackagesState = ParsePackagesState
  { parsePackagesAtto    :: T.Text -> Result (T.Text, Package)
  }

-- | Parses sequence of ByteStrings and tries to extract 'PackagesIndex' out of
-- it. When the stream is over and you have 'ParsePackagesPartial' result, you are
-- required to call continuation function with empty bytestring to get 'ParsePackagesDone'
-- or 'ParsePackagesFail'
parsePackages :: IncrementalParser B.ByteString PackagesIndex
parsePackages = parseUtf8 parseText
  where
    parseText :: IncrementalParser T.Text PackagesIndex
    parseText = go (PackagesIndex Map.empty) (ParsePackagesState $ parse parsePackage)
    go currentIdx (ParsePackagesState atto) inputText = case atto inputText of
      Done remainder (packageName, package) ->
        let newIdx = currentIdx & at packageName .~ Just package
        in  if T.null remainder
              then IncrementalParseOk (Just newIdx) (go newIdx (ParsePackagesState $ parse parsePackage))
              else go newIdx (ParsePackagesState $ parse parsePackage) remainder
      Partial next ->
        IncrementalParseOk (Just currentIdx) (go currentIdx $ ParsePackagesState next)
      Fail remainder ctx err ->
        IncrementalParseFail $ "Parsing failed with error " <> T.pack (show err) <> " contexts: " <> T.pack (show ctx) <> " remainder: " <> T.pack (show remainder)

    parsePackage :: Parser (T.Text, Package)
    parsePackage = do
      values <- keyValueMapParser parseSeparator
      name <- case values ^. at "Package" of
        Just v -> return v
        Nothing -> fail "Required field \"Package\" isn't present"
      filename <- case values ^. at "Filename" of
        Just v -> return v
        Nothing -> fail "Required field \"Filename\" isn't present"
      size <- case values ^. at "Size" of
        Just v -> return v
        Nothing -> fail "Required filed \"Size\" isn't present"
      return (name, Package filename size (values & sans "Package" . sans "Filename" . sans "Size"))

    parseSeparator :: Parser ()
    parseSeparator = endOfInput <|> endOfLine <|> (skipSpace *> endOfLine)

storePackages :: Monad m => PackagesIndex -> (B.ByteString -> m ()) -> m ()
storePackages idx act = forM_ (Map.toList $ _indexPackages idx) $ \(packageName, package) -> do
    yieldLine $ "Package: " <> packageName
    yieldLine $ "Filename: " <> _packageFilename package
    yieldLine $ "Size: " <> _packageSize package
    storeKeyValueMap (_packageOtherFields package) act
    yieldLine ""
  where
    yieldLine x = act $ T.encodeUtf8 (x <> "\r\n")
