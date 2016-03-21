module Main where

import Test.Tasty
import qualified Distribution.Debian.Repository.PackagesIndex.Tests

main :: IO ()
main = defaultMain $ testGroup "All tests"
  [ Distribution.Debian.Repository.PackagesIndex.Tests.tests
  ]
