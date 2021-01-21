module Zp (ZP, p) where

import GHC.Real
import GHC.Enum

newtype ZP = N Integer

c :: Integer
c = 208351617316091241234326746312124448251235562226470491514186331217050270460481

p :: ZP
p = fromInteger c

instance Num ZP where
  (+) (N a) (N b) = N $ mod (a + b) c
  (*) (N a) (N b) = N $ mod (a * b) c
  negate (N a)    = (N . (flip mod) c . negate) a
  fromInteger i   = (N . (flip mod) c . fromInteger) i
  signum (N a)    = (N . (flip mod) c . signum) a
  abs (N a)       = (N . (flip mod) c . abs) a
    

instance Show ZP where
  show (N a) = show a

instance Eq ZP where
  (==) (N a) (N b) = (mod a c) == (mod b c)

instance Enum ZP where
  toEnum         = fromIntegral
  fromEnum (N a) = fromEnum (mod a c)
 
instance Ord ZP where
  compare (N a) (N b) = compare (mod a c) (mod b c)

instance Real ZP where
  toRational (N a) = toRational (mod a c)

--Inverso multiplicativo
inverse :: ZP -> ZP
inverse n = n ^ (p - (N 2))

instance Fractional ZP where
  (/) n m = n * (inverse m)
  fromRational n = fromInteger (numerator n) / fromInteger (denominator n)
  
instance Integral ZP where
  quotRem (N a) (N b) = let (r, m) = quotRem (mod a c) (mod b c) in (N r, N m)
  toInteger (N a)     = a

