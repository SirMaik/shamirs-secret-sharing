module Misc
  ( ptsFromString
  , toByteString
  ) where

import Data.Binary
import Data.Bits
import qualified Data.ByteString as BS 

import Polynomial

ptsFromString :: String -> [Point]
ptsFromString = map ((\(a,b) -> (fromInteger a, fromInteger b)) . read) . lines


toByteString :: Integer -> BS.ByteString
toByteString = fillZeroes . BS.pack . reverse . convert
  where
    convert :: Integer -> [Word8]
    convert i = case divMod i 256 of
                  (0,0) -> []
                  (0,m) -> [fromInteger m]
                  (n,m) -> (fromInteger m) : convert n
    fillZeroes :: BS.ByteString -> BS.ByteString
    fillZeroes bs = let l = BS.length bs
                    in  BS.replicate (32 - l) zeroBits `BS.append` bs
