{-# LANGUAGE EmptyDataDecls, FlexibleContexts, OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Distribution.Debian.Repository.ReleaseFile
  ( ReleaseFile (..)
  , parseReleaseFile
  , MD5
  , HashedEntry (..)
  , releaseFileMd5Sum
  ) where

import Control.Lens
import Control.Lens.TH
import Data.Attoparsec.Text
import Data.Map.Strict (Map)
import Data.Monoid
import Distribution.Debian.Repository.Parse
import Prelude hiding (takeWhile)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data ReleaseFile = ReleaseFile
  { _releaseFileFields :: Map T.Text T.Text
  } deriving (Eq, Show)

newtype ParseReleaseState = ParseReleaseState
  { parseReleaseAtto :: T.Text -> Result (Map T.Text T.Text)
  }

makeLenses ''ReleaseFile

type instance Index ReleaseFile = T.Text
type instance IxValue ReleaseFile = T.Text

instance Ixed ReleaseFile where
  ix field = releaseFileFields . ix field

instance At ReleaseFile where
  at field = releaseFileFields . at field

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

data MD5

data HashedEntry hash = HashedEntry
  { hashedEntryHash     :: T.Text
  , hashedEntrySize     :: Int
  , hashedEntryFilename :: T.Text
  } deriving (Eq, Show)

parseMd5Sum :: T.Text -> [HashedEntry MD5]
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
      return $ HashedEntry hash size filename

storeMd5Sum :: [HashedEntry MD5] -> T.Text
storeMd5Sum hashes = T.intercalate "\n" $ map entryToLine hashes
  where
    entryToLine (HashedEntry hash size filename) =
      hash <> " " <> T.pack (show size) <> " " <> filename

releaseFileMd5Sum :: Lens' ReleaseFile (Maybe [HashedEntry MD5])
releaseFileMd5Sum = lens get put
  where get rf = parseMd5Sum <$> rf ^. at "MD5Sum"
        put rf v = rf & at "MD5Sum" .~ (storeMd5Sum <$> v)
