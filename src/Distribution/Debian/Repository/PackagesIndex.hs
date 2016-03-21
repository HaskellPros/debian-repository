{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Distribution.Debian.Repository.PackagesIndex
  ( Package (..)
  , PackagesIndex (..)
  , ParsePackagesResult (..)
  , parsePackages
  ) where

import Control.Applicative
import Control.Monad
import Control.Lens
import Control.Lens.TH
import Data.Attoparsec.Text
import Data.Map.Strict (Map)
import Data.Monoid
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

-- | Result data type for 'parsePackages' function.
data ParsePackagesResult
  = ParsePackagesOk PackagesIndex (B.ByteString -> ParsePackagesResult)
  -- ^ Previous parsing attempt was okay. First member is Packages Index that
  -- was parsed so far, second one is function to evaluate if more text gets
  -- available to continue parsing. Don't forget to call continuation function
  -- with empty bytestring when end-of-input has been reached.
  | ParsePackagesFail T.Text
  -- ^ Parsing attempt has failed with the given error.

data ParserState = ParserState
  { parserUtf8Decoding  :: B.ByteString -> T.Decoding
  , parsePackagesState  :: ParsePackagesState
  }

newtype ParsePackagesState = ParsePackagesState
  { parsePackagesAtto    :: T.Text -> Result (Maybe (T.Text, Package))
  }

-- | Parses sequence of ByteStrings and tries to extract 'PackagesIndex' out of
-- it. When the stream is over and you have 'ParsePackagesPartial' result, you are
-- required to call continuation function with empty bytestring to get 'ParsePackagesDone'
-- or 'ParsePackagesFail'
parsePackages :: B.ByteString -> ParsePackagesResult
parsePackages = go (PackagesIndex Map.empty) (ParserState T.streamDecodeUtf8 (ParsePackagesState $ parse packageParser))
  where
    -- Handle first layer of encoding - decode bytestrings to UTF-8 text and feed that to
    -- goText.
    go currentIdx (ParserState utf8Decoding st) input = case utf8Decoding input of
      T.Some text _ next ->
        if T.null text && not (B.null input)
          then ParsePackagesOk currentIdx (go currentIdx (ParserState next st))
          else case goText currentIdx st text of
                 Left e -> ParsePackagesFail e
                 Right (newIdx, newSt) -> if B.null input
                   then ParsePackagesOk newIdx (go newIdx (ParserState next (ParsePackagesState $ parse packageParser)))
                   else ParsePackagesOk newIdx (go newIdx (ParserState next newSt))
    -- Handle attoparsec parser for next layers
    goText :: PackagesIndex -> ParsePackagesState -> T.Text -> Either T.Text (PackagesIndex, ParsePackagesState)
    goText currentIdx (ParsePackagesState atto) inputText = case atto inputText of
      Done remainder result -> case result of
        Just (packageName, package) ->
          let newIdx = currentIdx & at packageName .~ Just package
          in  if T.null remainder
                then Right (newIdx, ParsePackagesState $ parse packageParser)
                else goText newIdx (ParsePackagesState $ parse packageParser) remainder
      Partial next ->
        Right (currentIdx, ParsePackagesState next)
      Fail remainder ctx err ->
        Left $ "Parsing failed with error " <> T.pack (show err) <> " contexts: " <> T.pack (show ctx) <> " remainder: " <> T.pack (show remainder)

    -- Returns Just value for successfully parsed package info, Nothing for successfully parsed
    -- package separator
    packageParser :: Parser (Maybe (T.Text, Package))
    packageParser = (parseSeparator *> pure Nothing <?> "parse-separator") <|> (Just <$> parsePackage <?> "parse-package")

    parsePackage :: Parser (T.Text, Package)
    parsePackage = do
      (key, value) <- keyValueParser
      if key == "Package"
        then (,) <$> pure value <*> packageFieldsParser
        else fail "First field of package description must be named \"Package\""

    parseSeparator :: Parser ()
    parseSeparator = endOfInput <|> endOfLine <|> (skipSpace *> endOfLine)

    packageFieldsParser :: Parser Package
    packageFieldsParser = do
        values <- parseValues Map.empty
        filename <- case values ^. at "Filename" of
          Just v -> return v
          Nothing -> fail "Required field \"Filename\" isn't present"
        size <- case values ^. at "Size" of
          Just v -> return v
          Nothing -> fail "Required filed \"Size\" isn't present"
        return $ Package filename size (values & sans "Filename" . sans "Size")
      where
        parseValues current = do
          maybeParse <-
                (parseSeparator *> pure Nothing <?> "parse-separator")
            <|> (Just <$> keyValueParser <?> "parse-key-value")
          case maybeParse of
            Just (k, v) -> parseValues $ Map.insert k v current
            Nothing -> pure current

    keyValueParser :: Parser (T.Text, T.Text)
    keyValueParser = do
      skipSpace
      key <- takeWhile1 (/= ':')
      case key of
        "Description" -> (,) <$> pure key <*> descriptionParser
        _ -> do
          char ':'
          skipSpace
          value <- takeWhile (not . isEndOfLine)
          endOfLineOrInput
          return (key, value)

    -- | Debian package format is special for "Description" field values (Debian policy 5.6.13)
    descriptionParser :: Parser T.Text
    descriptionParser = do
      char ':'
      skipSpace
      synopsis <- takeWhile (not . isEndOfLine)
      endOfLineOrInput
      remainder <- remainingLines []
      return $ T.intercalate "\n" (synopsis:remainder)
      where
        remainingLines current = do
          maybeNextLine <- option
            Nothing
            (Just <$> (spaceAndStop <|> singleSpaceLine <|> doubleSpaceLine))
          case maybeNextLine of
            Nothing -> return current
            Just l -> remainingLines (current ++ [l])
        spaceAndStop = do
          space
          char '.'
          endOfLine
          return ""
        singleSpaceLine = do
          space
          restOfLine
        doubleSpaceLine = do
          space
          void $ takeWhile1 isHorizontalSpace
          restOfLine
        restOfLine = do
          line <- takeWhile (not . isEndOfLine)
          endOfLineOrInput
          return line

    endOfLineOrInput = endOfLine <|> endOfInput
