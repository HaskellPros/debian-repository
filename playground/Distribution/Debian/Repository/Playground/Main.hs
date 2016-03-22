{-# LANGUAGE OverloadedStrings #-}
module Distribution.Debian.Repository.Playground.Main
  ( main
  ) where

import Distribution.Debian.Repository

main :: IO ()
main = do
  repo <- loadRepository "http://ftp.debian.org/debian" "wheezy" ["main", "contrib", "non-free"]
  print repo
