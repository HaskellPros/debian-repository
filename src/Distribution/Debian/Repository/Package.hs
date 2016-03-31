{-# LANGUAGE FlexibleContexts, OverloadedStrings, TypeFamilies, TemplateHaskell #-}
module Distribution.Debian.Repository.Package
  ( Package (..)
  , packageTasks
  , DebianVersion (..)
  , packageVersion
  , packageVersionRaw
  , parseDebianVersion'
  ) where

import Control.Lens
import Control.Lens.TH
import Data.Attoparsec.Text
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Debian.Version
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Text.PrettyPrint as Pretty

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

packageVersion :: Lens' Package (Maybe DebianVersion)
packageVersion = lens get put
  where get package = (parseDebianVersion' . T.unpack) <$> package ^. at "Version"
        put package version = package & at "Version" .~ ((T.pack . Pretty.render . prettyDebianVersion) <$> version)

packageVersionRaw :: Lens' Package (Maybe T.Text)
packageVersionRaw = at "Version"
