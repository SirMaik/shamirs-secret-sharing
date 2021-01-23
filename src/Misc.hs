{-|
Module      : Misc
Description : Funciones extras.
Copyright   : -
License     : -
Maintainer  : mianorsi@ciencias.unam.mx
Stability   : experimental
Portability : POSIX

Aquí se encuentran las funciones que no encajan en otros módulos
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


-- | Función que convierte una cadena en una lista de puntos.
ptsFromString :: String  -- ^ Cadena.
              -> [Point] -- ^ Lista de puntos.
ptsFromString = map ((\(a,b) -> (fromInteger a, fromInteger b)) . read) . lines


-- | Función que convierte un número en Zp en un ByteString.
toByteString :: ZP            -- ^ Número en Zp.
             -> BS.ByteString -- ^ Bytestring. 
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
