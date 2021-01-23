{-|
Module      : Zp
Description : Módulo correspondiente al campo Zp.
Copyright   : -
License     : -
Maintainer  : mianorsi@ciencias.unam.mx
Stability   : experimental
Portability : POSIX

Aquí se encuentra todo lo correspondiente a las operaciones en Zp.
-}
module Zp (ZP, p) where

import GHC.Real
import GHC.Enum

-- | Tipo de dato que corresponde a un número en Zp.
newtype ZP = N Integer

-- | El primo de 256 bits que estamos utilizando.
c :: Integer
c = 208351617316091241234326746312124448251235562226470491514186331217050270460481

-- | El primo representado en el campo.
p :: ZP
p = fromInteger c

-- | Instancia de la clase Num.
instance Num ZP where
  (+) (N a) (N b) = N $ mod (a + b) c
  (*) (N a) (N b) = N $ mod (a * b) c
  negate (N a)    = (N . (flip mod) c . negate) a
  fromInteger i   = (N . (flip mod) c . fromInteger) i
  signum (N a)    = (N . (flip mod) c . signum) a
  abs (N a)       = (N . (flip mod) c . abs) a
    
-- | Instancia de la clase Show.
instance Show ZP where
  show (N a) = show a

-- | Instancia de la clase Eq.
instance Eq ZP where
  (==) (N a) (N b) = (mod a c) == (mod b c)

-- | Instancia de la clase Enum.
instance Enum ZP where
  toEnum         = fromIntegral
  fromEnum (N a) = fromEnum (mod a c)

-- | Instancia de la clase Ord.
instance Ord ZP where
  compare (N a) (N b) = compare (mod a c) (mod b c)

-- | instancia de la clase Real.
instance Real ZP where
  toRational (N a) = toRational (mod a c)

-- | Calcula el inverso multiplicativo de un número en Zp.
inverse :: ZP -- ^ El número.
        -> ZP -- ^ Su inverso multiplicativo en Zp.
inverse n = n ^ (p - (N 2))

-- | Instancia de la clase Fracional.
instance Fractional ZP where
  (/) n m = n * (inverse m)
  fromRational n = fromInteger (numerator n) / fromInteger (denominator n)

-- | Instancia de la clase Integral.  
instance Integral ZP where
  quotRem (N a) (N b) = let (r, m) = quotRem (mod a c) (mod b c) in (N r, N m)
  toInteger (N a)     = a

