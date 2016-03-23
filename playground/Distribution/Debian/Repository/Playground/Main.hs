{-# LANGUAGE OverloadedStrings #-}
module Distribution.Debian.Repository.Playground.Main
  ( main
  ) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Distribution.Debian.Repository

main :: IO ()
main = runStdoutLoggingT $ do
  repo <- loadRepository LoadRepositoryArgs
    { loadRepositoryUri = "http://ftp.acc.umu.se/debian"
    , loadRepositoryDist = "wheezy"
    , loadRepositoryComponents = ["main", "contrib", "non-free"]
    , loadRepositoryArchitecture = "amd64"
    }
  liftIO $ print repo
