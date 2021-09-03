{-|
Module      : Misc
Description : Extra functions
Copyright   : -
License     : -
Maintainer  : mianorsi@ciencias.unam.mx
Stability   : experimental
Portability : POSIX

Here are all the functions that don't fit in the rest of the modules.
-}
module Misc
  ( ptsFromString
  , toByteString
  ) where

import Data.Binary
import Data.Bits
import qualified Data.ByteString as BS 

import Zp
import Polynomial


-- | Function that transforms a String into a List of Points
ptsFromString :: String  -- ^ The String
              -> [Point] -- ^ A List of Points
ptsFromString = map ((\(a,b) -> (fromInteger a, fromInteger b)) . read) . lines


-- | Function that transforms a number in Zp into a ByteString
toByteString :: ZP            -- ^ Number in Zp
             -> BS.ByteString -- ^ ByteString
toByteString = fillZeroes . BS.pack . reverse . convert . toInteger
  where
    convert :: Integer -> [Word8]
    convert i = case divMod i 256 of
                  (0,0) -> []
                  (0,m) -> [fromInteger m]
                  (n,m) -> (fromInteger m) : convert n
    fillZeroes :: BS.ByteString -> BS.ByteString
    fillZeroes bs = let l = BS.length bs
                    in  BS.replicate (32 - l) zeroBits `BS.append` bs
