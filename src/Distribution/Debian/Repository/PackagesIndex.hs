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
import Distribution.Debian.Repository.Parse
import Prelude hiding (takeWhile)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Basic data type for a repository Package Index entry. Implements 'Control.Lens.At.Ixed'
-- and 'Control.Lens.At.At' in such a way that all attributes can be accessed using their
-- names. "Filename" and "Size" should always be present, other ones are optional.
data Package = Package
  { _packageFilename    :: T.Text
  , _packageSize        :: T.Text
  , _packageOtherFields :: Map T.Text T.Text
  } deriving (Eq, Show)

data PackagesIndex = PackagesIndex
  { _indexPackages :: Map T.Text Package
  } deriving (Eq, Show)

makeLenses ''Package
makeLenses ''PackagesIndex

type instance Index Package = T.Text
type instance IxValue Package = T.Text

instance Ixed Package where
  ix field
    | field == "Filename" = packageFilename
    | field == "Size" = packageSize
    | otherwise = packageOtherFields . ix field

type instance Index PackagesIndex = T.Text
type instance IxValue PackagesIndex = Package

instance At Package where
  at field
    | field == "Filename" = lens
        (Just . _packageFilename)
        (\p mv -> case mv of
          Just v -> p {_packageFilename = v}
          Nothing -> error "Filename field is mandatory for Packages"
        )
    | field == "Size" = lens
        (Just . _packageSize)
        (\p mv -> case mv of
          Just v -> p {_packageSize = v}
          Nothing -> error "Size field is mandatory for Packages"
        )
    | otherwise = packageOtherFields . at field

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
    forM_ (Map.toList $ _packageOtherFields package) $ \(fieldName, fieldValue) -> case fieldName of
      "Description" -> storeDescription fieldValue
      _ -> yieldLine $ fieldName <> ": " <> fieldValue
    yieldLine ""
  where
    yieldLine x = act $ T.encodeUtf8 (x <> "\r\n")
    storeDescription v = do
      let descriptionLines = T.splitOn "\n" v
          (firstLine, nextLines) = case descriptionLines of
            []   -> (T.empty, [])
            x:xs -> (x, xs)
      yieldLine $ "Description: " <> firstLine
      forM_ nextLines $ \line ->
        if T.null line
          then yieldLine " ."
          else yieldLine $ " " <> line
