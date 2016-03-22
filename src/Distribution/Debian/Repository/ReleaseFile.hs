{-# LANGUAGE OverloadedStrings #-}
module Distribution.Debian.Repository.ReleaseFile
  ( ReleaseFile (..)
  , parseReleaseFile
  ) where

import Data.Attoparsec.Text
import Data.Map.Strict (Map)
import Data.Monoid
import Distribution.Debian.Repository.Parse
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data ReleaseFile = ReleaseFile
  { _releaseFileFields :: Map T.Text T.Text
  } deriving (Eq, Show)

newtype ParseReleaseState = ParseReleaseState
  { parseReleaseAtto :: T.Text -> Result (Map T.Text T.Text)
  }

parseReleaseFile :: IncrementalParser B.ByteString ReleaseFile
parseReleaseFile = parseUtf8 parseText
  where
    parseText = go (ParseReleaseState $ parse releaseParser)
    go (ParseReleaseState atto) inputText = case atto inputText of
      Done remainder fields ->
        IncrementalParseOk
          (Just $ ReleaseFile fields)
          (go (ParseReleaseState $ parse releaseParser))
      Partial next ->
        IncrementalParseOk Nothing (go $ ParseReleaseState next)
      Fail remainder ctx err ->
        IncrementalParseFail $ "Parsing failed with error " <> T.pack (show err) <> " contexts: " <> T.pack (show ctx) <> " remainder: " <> T.pack (show remainder)
    releaseParser = keyValueMapParser endOfInput
