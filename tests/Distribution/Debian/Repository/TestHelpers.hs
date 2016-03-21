module Distribution.Debian.Repository.TestHelpers (gRandomStringChunks) where

import Data.List (nub, sort)
import Test.QuickCheck
import qualified Data.ByteString as B

gRandomStringChunks :: B.ByteString -> Gen [B.ByteString]
gRandomStringChunks input = do
  positions <- fmap (nub . sort) . listOf $ choose (1, B.length input - 1)
  return $ multiSplit positions input

multiSplit :: [Int] -> B.ByteString -> [B.ByteString]
multiSplit indices bs = reverse $ go (reverse indices) bs
  where
    go (i:is) bs =
      let (x, y) = B.splitAt i bs
      in  y:go is x
    go [] bs = [bs]
