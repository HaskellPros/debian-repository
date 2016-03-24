{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, TemplateHaskell #-}
module Distribution.Debian.Repository.Package
  ( Package (..)
  , packageTasks
  , PackageVersion (..)
  , packageVersion
  , packageVersionRaw
  ) where

import Control.Lens
import Control.Lens.TH
import Data.Attoparsec.Text
import Data.Map.Strict (Map)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

-- | Basic data type for a repository Package Index entry. Implements 'Control.Lens.At.Ixed'
-- and 'Control.Lens.At.At' in such a way that all attributes can be accessed using their
-- names. "Filename" and "Size" should always be present, other ones are optional.
data Package = Package
  { _packageFilename    :: T.Text
  , _packageSize        :: T.Text
  , _packageOtherFields :: Map T.Text T.Text
  } deriving (Eq, Show)

makeLenses ''Package

type instance Index Package = T.Text
type instance IxValue Package = T.Text

instance Ixed Package where
  ix field
    | field == "Filename" = packageFilename
    | field == "Size" = packageSize
    | otherwise = packageOtherFields . ix field

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

packageTasks :: Package -> [T.Text]
packageTasks package = case package ^. at "Task" of
  Nothing -> []
  Just tasks -> map (T.dropWhile (== ' ')) (T.splitOn "," tasks)

data PackageVersion = PackageVersion
  { packageVersionEpoch    :: Maybe Int
  , packageVersionUpstream :: T.Text
  , packageVersionDebian   :: Maybe T.Text
  } deriving (Eq, Show)

packageVersion :: Lens' Package (Maybe PackageVersion)
packageVersion = lens get put
  where get package = package ^. at "Version" >>= packageVersionParse
        put package version = package & at "Version" .~ (packageVersionText <$> version)

packageVersionRaw :: Lens' Package (Maybe T.Text)
packageVersionRaw = at "Version"

packageVersionText :: PackageVersion -> T.Text
packageVersionText ver =
     (case packageVersionEpoch ver of
        Just e -> T.pack (show e) <> ":"
        Nothing -> "")
  <> packageVersionUpstream ver
  <> (case packageVersionDebian ver of
        Just vd -> "-" <> vd
        Nothing -> "")

packageVersionParse :: T.Text -> Maybe PackageVersion
packageVersionParse t = case parseOnly versionParser t of
    Left _ -> Nothing
    Right x -> Just x
  where
    versionParser = do
      epoch <- option Nothing (Just <$> parseEpoch)
      upstream <- takeWhile1 (/= '-')
      debian <- option Nothing (Just <$> parseDebian)
      return $ PackageVersion epoch upstream debian
    parseEpoch = do
      e <- decimal
      char ':'
      return e
    parseDebian = do
      char '-'
      takeText
