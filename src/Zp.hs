{-|
Module      : Zp
Description : The field Zp
Copyright   : -
License     : -
Maintainer  : mianorsi@ciencias.unam.mx
Stability   : experimental
Portability : POSIX

Here are the definitions that correspond to the field Zp.
-}
module Zp (ZP, p) where

import GHC.Real
import GHC.Enum

-- | Type which models Zp
newtype ZP = N Integer

-- | This is the 256 bits prime number which is going to be used
c :: Integer
c = 208351617316091241234326746312124448251235562226470491514186331217050270460481

-- | The prime number represented in the field.
p :: ZP
p = fromInteger c

-- | Zp's instance of the the class Num
instance Num ZP where
  (+) (N a) (N b) = N $ mod (a + b) c
  (*) (N a) (N b) = N $ mod (a * b) c
  negate (N a)    = (N . (flip mod) c . negate) a
  fromInteger i   = (N . (flip mod) c . fromInteger) i
  signum (N a)    = (N . (flip mod) c . signum) a
  abs (N a)       = (N . (flip mod) c . abs) a
    
-- | Zp's instance of the the class Show
instance Show ZP where
  show (N a) = show a

-- | Zp's instance of the the class Eq
instance Eq ZP where
  (==) (N a) (N b) = (mod a c) == (mod b c)

-- | Zp's instance of the the class Enum
instance Enum ZP where
  toEnum         = fromIntegral
  fromEnum (N a) = fromEnum (mod a c)

-- | Zp's instance of the the class Ord
instance Ord ZP where
  compare (N a) (N b) = compare (mod a c) (mod b c)

-- | Zp's instance of the the class Real
instance Real ZP where
  toRational (N a) = toRational (mod a c)

-- | Function that calculates the multiplicative inverse of a number in Zp
inverse :: ZP -- ^ The number
        -> ZP -- ^ Its multiplicative inverse in Zp
inverse n = n ^ (p - (N 2))

-- | Zp's instance of the the class Fractional
instance Fractional ZP where
  (/) n m = n * (inverse m)
  fromRational n = fromInteger (numerator n) / fromInteger (denominator n)

-- | Zp's instance of the the class Integral
instance Integral ZP where
  quotRem (N a) (N b) = let (r, m) = quotRem (mod a c) (mod b c) in (N r, N m)
  toInteger (N a)     = a

