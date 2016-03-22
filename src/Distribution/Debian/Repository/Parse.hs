{-# LANGUAGE OverloadedStrings #-}
module Distribution.Debian.Repository.Parse
  ( IncrementalParseResult (..)
  , IncrementalParser (..)
  , parseUtf8
  , keyValueMapParser
  ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Map.Strict
import Prelude hiding (takeWhile)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Result data type for 'parsePackages' function.
data IncrementalParseResult i r
  = IncrementalParseOk (Maybe r) (i -> IncrementalParseResult i r)
  -- ^ Previous parsing attempt was okay. First member is Packages Index that
  -- was parsed so far, second one is function to evaluate if more text gets
  -- available to continue parsing. Don't forget to call continuation function
  -- with empty bytestring when end-of-input has been reached.
  | IncrementalParseFail T.Text
  -- ^ Parsing attempt has failed with the given error.

type IncrementalParser i r = i -> IncrementalParseResult i r

data ParseUtf8State r = ParseUtf8State
  { parseUtf8Decode      :: B.ByteString -> T.Decoding
  , parseUtf8InnerResult :: IncrementalParser T.Text r
  }

parseUtf8 :: Show r => IncrementalParser T.Text r -> IncrementalParser B.ByteString r
parseUtf8 textParser = go Nothing (ParseUtf8State T.streamDecodeUtf8 textParser)
  where
    go r (ParseUtf8State utf8Decoding inner) input = case utf8Decoding input of
      T.Some text _ next ->
        if T.null text && not (B.null input)
          then IncrementalParseOk r (go r (ParseUtf8State next inner))
          else case inner text of
                 IncrementalParseFail e -> IncrementalParseFail e
                 IncrementalParseOk innerR innerParse ->
                   IncrementalParseOk innerR (go innerR (ParseUtf8State next innerParse))

-- | Accepts a parser to signal end of key-value data and parses list of key-value
-- pairs into a map.
keyValueMapParser :: Parser () -> Parser (Map T.Text T.Text)
keyValueMapParser parseEndOfMap = parseValues Map.empty
  where
    parseValues current = do
      maybeParse <-
            (parseEndOfMap *> pure Nothing <?> "parse-end-of-map")
        <|> (Just <$> keyValueParser <?> "parse-key-value")
      case maybeParse of
        Just (k, v) -> parseValues $ Map.insert k v current
        Nothing -> pure current
    keyValueParser :: Parser (T.Text, T.Text)
    keyValueParser = do
      skipSpace
      key <- takeWhile1 (/= ':')
      char ':'
      skipSpace
      value <- parseValue
      return (key, value)
    parseValue = do
      firstLine <- takeWhile (not . isEndOfLine)
      endOfLine
      isAtEnd <- atEnd
      if isAtEnd
        then return firstLine
        else do
          remainder <- remainingLines []
          return $ T.intercalate "\n" (firstLine:remainder)
      where
        remainingLines current = do
          maybeNextLine <- option
            Nothing
            (Just <$> (spaceAndStop <|> singleSpaceLine <|> doubleSpaceLine))
          case maybeNextLine of
            Nothing -> return current
            Just l -> remainingLines (current ++ [l])
        spaceAndStop = do
          char ' '
          char '.'
          endOfLine <|> endOfInput
          return ""
        singleSpaceLine = do
          char ' '
          restOfLine
        doubleSpaceLine = do
          char ' '
          void $ takeWhile1 isHorizontalSpace
          restOfLine
        restOfLine = do
          line <- takeWhile (not . isEndOfLine)
          endOfLine <|> endOfInput
          return line
