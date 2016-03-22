{-# LANGUAGE OverloadedStrings #-}
module Distribution.Debian.Repository.PackagesIndex.Tests (tests) where

import Control.Monad.ST
import Data.Map.Strict (Map)
import Data.Monoid
import Data.STRef
import Distribution.Debian.Repository.PackagesIndex
import Distribution.Debian.Repository.TestHelpers
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

parse2UbuntuPackages :: IO ()
parse2UbuntuPackages =
  case parsePackages ubuntuPackages of
    IncrementalParseOk _ next -> case next B.empty of
      IncrementalParseOk idx _ -> assertEqual "Parse result equals expected" (Just ubuntuPackagesParsed) idx
      IncrementalParseFail msg -> assertFailure $ "Parse failed with error message " <> T.unpack msg
    IncrementalParseFail msg -> assertFailure $ "Parse failed with error message " <> T.unpack msg

parse2UbuntuRandomChunks :: Property
parse2UbuntuRandomChunks = forAll (gRandomStringChunks ubuntuPackages) $ \chunks ->
    case chunks of
      [] -> True
      x:xs -> case foldl foldFn (parsePackages x) xs of
        IncrementalParseOk _ next -> case next B.empty of
          IncrementalParseOk idx _ -> Just ubuntuPackagesParsed == idx
          IncrementalParseFail msg -> False
        IncrementalParseFail msg -> False
  where
    foldFn res chunk = case res of
      IncrementalParseOk _ next -> next chunk
      IncrementalParseFail _ -> res

backAndForth2Ubuntu :: IO ()
backAndForth2Ubuntu = do
  let serialized = runST $ do
        ref <- newSTRef B.empty
        storePackages ubuntuPackagesParsed $ \chunk -> do
          current <- readSTRef ref
          writeSTRef ref (current <> chunk)
        readSTRef ref
  case parsePackages serialized of
    IncrementalParseOk _ next -> case next B.empty of
      IncrementalParseOk idx _ -> assertEqual "Parse result equals expected" (Just ubuntuPackagesParsed) idx
      IncrementalParseFail msg -> assertFailure $ "Parse failed with error message " <> T.unpack msg
    IncrementalParseFail msg -> assertFailure $ "Parse failed with error message " <> T.unpack msg

tests :: TestTree
tests = testGroup "Distribution.Debian.Repository.PackagesIndex.Tests"
  [ testCase "Parse 2 Ubuntu packages" parse2UbuntuPackages
  , testProperty "Any chunks 2 Ubuntu packages" parse2UbuntuRandomChunks
  , testCase "Back and forth 2 Ubuntu packages" backAndForth2Ubuntu
  ]

ubuntuPackages :: B.ByteString
ubuntuPackages = B.intercalate "\r\n"
  [ "Package: accountsservice"
  , "Priority: standard"
  , "Section: gnome"
  , "Installed-Size: 428"
  , "Maintainer: Ubuntu Developers <ubuntu-devel-discuss@lists.ubuntu.com>"
  , "Original-Maintainer: Alessio Treglia <alessio@debian.org>"
  , "Architecture: amd64"
  , "Version: 0.6.35-0ubuntu7.2"
  , "Depends: dbus, libaccountsservice0 (= 0.6.35-0ubuntu7.2), libc6 (>= 2.4), libgcr-base-3-1 (>= 3.8.0), libglib2.0-0 (>= 2.37.3), libpam0g (>= 0.99.7.1), libpolkit-gobject-1-0 (>= 0.99)"
  , "Suggests: gnome-control-center"
  , "Filename: pool/main/a/accountsservice/accountsservice_0.6.35-0ubuntu7.2_amd64.deb"
  , "Size: 60388"
  , "MD5sum: e44935e8ff4d5c086500d4e956e0e852"
  , "SHA1: e38b1479bfb605c2ff18befe8f97ce206fe46269"
  , "SHA256: 668e02bce93ac1d3ab63d70569b3d60b803ad67333151bf4bb007acdcd717cce"
  , "SHA512: 8ae36e8b9399fb1453eb38a0cc6b1bc4d224c054bf281edb2a1093462cf196c67bdc3bf069cfbb32f8e5b1f972b1d0d7b733e157b30334167c68a3a5e2d3dda6"
  , "Description: query and manipulate user account information"
  , " The AccountService project provides a set of D-Bus"
  , " interfaces for querying and manipulating user account"
  , " information and an implementation of these interfaces,"
  , " based on the useradd, usermod and userdel commands."
  , "Homepage: http://cgit.freedesktop.org/accountsservice/"
  , "Bugs: https://bugs.launchpad.net/ubuntu/+filebug"
  , "Origin: Ubuntu"
  , "Supported: 5y"
  , "Task: standard"
  , ""
  , "Package: libaccountsservice0"
  , "Priority: standard"
  , "Section: libs"
  , "Installed-Size: 365"
  , "Maintainer: Ubuntu Developers <ubuntu-devel-discuss@lists.ubuntu.com>"
  , "Original-Maintainer: Alessio Treglia <alessio@debian.org>"
  , "Architecture: amd64"
  , "Source: accountsservice"
  , "Version: 0.6.35-0ubuntu7.2"
  , "Depends: libc6 (>= 2.2.5), libgcr-base-3-1 (>= 3.8.0), libglib2.0-0 (>= 2.37.3), libsystemd-login0 (>= 186)"
  , "Breaks: accountsservice (<< 0.6.5)"
  , "Filename: pool/main/a/accountsservice/libaccountsservice0_0.6.35-0ubuntu7.2_amd64.deb"
  , "Size: 69658"
  , "MD5sum: 59889f8c9b26c57ae7a6349981a85749"
  , "SHA1: 5b5fd5b7ee9770773f84553d22ebde1c53fbd5af"
  , "SHA256: ffc30b100d645a08072d28865c6a7bcb1222160b8e3a21be6e7b1a1b42398eb9"
  , "SHA512: 47f55d4dc220a661b3a549a6ec61ea81bc14daa5d3090f8c299b1a397eaea50cd62490c30ece26ffd283c6607b3c4e85e13ef18d8cc260b379fc9c5f0a71b726"
  , "Description: query and manipulate user account information - shared libraries"
  , " The AccountService project provides a set of D-Bus"
  , " interfaces for querying and manipulating user account"
  , " information and an implementation of these interfaces,"
  , " based on the useradd, usermod and userdel commands."
  , " ."
  , " This package provides the shared libraries of the"
  , " AccountService library."
  , "Multi-Arch: same"
  , "Homepage: http://cgit.freedesktop.org/accountsservice/"
  , "Bugs: https://bugs.launchpad.net/ubuntu/+filebug"
  , "Origin: Ubuntu"
  , "Supported: 5y"
  , "Task: standard"
  , ""
  ]

ubuntuPackagesParsed :: PackagesIndex
ubuntuPackagesParsed = PackagesIndex $ Map.fromList
  [ ("accountsservice", Package
      { _packageFilename = "pool/main/a/accountsservice/accountsservice_0.6.35-0ubuntu7.2_amd64.deb"
      , _packageSize = "60388"
      , _packageOtherFields = Map.fromList
        [ ("Priority", "standard")
        , ("Section", "gnome")
        , ("Installed-Size", "428")
        , ("Maintainer", "Ubuntu Developers <ubuntu-devel-discuss@lists.ubuntu.com>")
        , ("Original-Maintainer", "Alessio Treglia <alessio@debian.org>")
        , ("Architecture", "amd64")
        , ("Version", "0.6.35-0ubuntu7.2")
        , ("Depends", "dbus, libaccountsservice0 (= 0.6.35-0ubuntu7.2), libc6 (>= 2.4), libgcr-base-3-1 (>= 3.8.0), libglib2.0-0 (>= 2.37.3), libpam0g (>= 0.99.7.1), libpolkit-gobject-1-0 (>= 0.99)")
        , ("Suggests", "gnome-control-center")
        , ("MD5sum", "e44935e8ff4d5c086500d4e956e0e852")
        , ("SHA1", "e38b1479bfb605c2ff18befe8f97ce206fe46269")
        , ("SHA256", "668e02bce93ac1d3ab63d70569b3d60b803ad67333151bf4bb007acdcd717cce")
        , ("SHA512", "8ae36e8b9399fb1453eb38a0cc6b1bc4d224c054bf281edb2a1093462cf196c67bdc3bf069cfbb32f8e5b1f972b1d0d7b733e157b30334167c68a3a5e2d3dda6")
        , ("Description", T.intercalate "\n"
            [ "query and manipulate user account information"
            , "The AccountService project provides a set of D-Bus"
            , "interfaces for querying and manipulating user account"
            , "information and an implementation of these interfaces,"
            , "based on the useradd, usermod and userdel commands."
            ])
        , ("Homepage", "http://cgit.freedesktop.org/accountsservice/")
        , ("Bugs", "https://bugs.launchpad.net/ubuntu/+filebug")
        , ("Origin", "Ubuntu")
        , ("Supported", "5y")
        , ("Task", "standard")
        ]
      }
    )
  , ("libaccountsservice0", Package
      { _packageFilename = "pool/main/a/accountsservice/libaccountsservice0_0.6.35-0ubuntu7.2_amd64.deb"
      , _packageSize = "69658"
      , _packageOtherFields = Map.fromList
        [ ("Priority", "standard")
        , ("Section", "libs")
        , ("Installed-Size", "365")
        , ("Maintainer", "Ubuntu Developers <ubuntu-devel-discuss@lists.ubuntu.com>")
        , ("Original-Maintainer", "Alessio Treglia <alessio@debian.org>")
        , ("Architecture", "amd64")
        , ("Source", "accountsservice")
        , ("Version", "0.6.35-0ubuntu7.2")
        , ("Depends", "libc6 (>= 2.2.5), libgcr-base-3-1 (>= 3.8.0), libglib2.0-0 (>= 2.37.3), libsystemd-login0 (>= 186)")
        , ("Breaks", "accountsservice (<< 0.6.5)")
        , ("MD5sum", "59889f8c9b26c57ae7a6349981a85749")
        , ("SHA1", "5b5fd5b7ee9770773f84553d22ebde1c53fbd5af")
        , ("SHA256", "ffc30b100d645a08072d28865c6a7bcb1222160b8e3a21be6e7b1a1b42398eb9")
        , ("SHA512", "47f55d4dc220a661b3a549a6ec61ea81bc14daa5d3090f8c299b1a397eaea50cd62490c30ece26ffd283c6607b3c4e85e13ef18d8cc260b379fc9c5f0a71b726")
        , ("Description", T.intercalate "\n"
            [ "query and manipulate user account information - shared libraries"
            , "The AccountService project provides a set of D-Bus"
            , "interfaces for querying and manipulating user account"
            , "information and an implementation of these interfaces,"
            , "based on the useradd, usermod and userdel commands."
            , ""
            , "This package provides the shared libraries of the"
            , "AccountService library."
            ])
        , ("Multi-Arch", "same")
        , ("Homepage", "http://cgit.freedesktop.org/accountsservice/")
        , ("Bugs", "https://bugs.launchpad.net/ubuntu/+filebug")
        , ("Origin", "Ubuntu")
        , ("Supported", "5y")
        , ("Task", "standard")
        ]
      }
    )
  ]
