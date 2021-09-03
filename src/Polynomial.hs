{-|
Module      : Polynomial
Description : Handling of Polynomials
Copyright   : -
License     : -
Maintainer  : mianorsi@ciencias.unam.mx
Stability   : experimental
Portability : POSIX

Here are the definitions that correspond to the handling of polynomials.
-}
module Polynomial
  ( X, Y, Point,
    randPoly,
    randPoints,
    lagrange,
    horner
  ) where

import Data.List
import Crypto.Number.Generate
import Crypto.Random
  
import Zp 

-- | x coordinate
type X = ZP
-- | y coordinate
type Y = ZP
-- | A point (x,y). This is the way in which shares are represented.
type Point = (X, Y)


-- | The degree of the polynomial
type Degree = Integer
-- | The coeficients of the polynomial (They belong in Zp)
type Coefficient = ZP
-- | The polynomial is modeled using a list of Coefficients
-- If the polynomial is a*x^n + ... + b*x + c then the corresponding list is [c, b, ..., a]
type Polynomial = [Coefficient]

-- | Function that returns the degree of a polynomial
degree :: Polynomial -- ^ The polynomial
       -> Degree     -- ^ The degree of the polynomial
degree = genericLength

-- | Auxiliar function that checks if a list has dupicates
-- It's going to be used when we generate a list of random unduplicated numbers
hasDuplicates :: Ord a => [a]  -- ^ The list
                       -> Bool -- ^ If the list has duplicates or not
hasDuplicates xs = f (sort xs) where
  f (x : t@(y : _)) = x == y || f t
  f _ = False


-- | Function that generates a random polinomial with unduplicated coeficients
randPoly :: (DRG gen) => gen               -- ^ Random number generator
                      -> ZP                -- ^ The constant term of the polynomial
                      -> Degree            -- ^ The degree of the polynomial
                      -> (Polynomial, gen) -- ^ (A random polynomial, a new random number generator)
randPoly drg const deg =  let l    = genericTake deg $ iterate (genRandom . snd) (genRandom drg)
                              drg' = (snd . last) l
                              poly =  map fst l
                          in if hasDuplicates poly
                             then randPoly drg' const deg
                             else (const:poly, drg')

-- | Function that generates a random list of unduplicate points which correspond to certain polynomial
randPoints :: (DRG gen) => gen            -- ^ Random number generator
                        -> Polynomial     -- ^ The polynomial
                        -> Integer        -- ^ The number of points
                        -> ([Point], gen) -- ^ (The list of random unduplicate points, a new random number generator)
randPoints drg poly n = let l    = genericTake n $ iterate (genRandom . snd) (genRandom drg)
                            drg' = (snd . last) l
                            xs   =  map fst l 
                        in if hasDuplicates xs
                           then randPoints drg' poly n
                           else (map (\x -> (x, horner poly x)) xs, drg')
                             

-- | Lagrange's interpolation method
-- It evaluates the corresponding polynomial from a list of points in a value x
lagrange :: [Point] -- ^ The list of points
         -> X       -- ^ The value x
         -> Y       -- ^ The result of the evaluation
lagrange pts x = sum $ map (\p1@(x1,y1) -> (\r -> y1 * r) (basePol p1)) pts
  where basePol pi@(xi,_) = foldr (\p2@(x2,_) r -> if pi /= p2 then ((x-x2)/(xi-x2))*r else r) 1 pts

  
-- | Horner's evaluation method
-- It evaluates a polynomial in a value x
horner :: Polynomial -- ^ The polynomial
       -> X          -- ^ The value x
       -> Y          -- ^ The result of the evaluation
horner poly x = foldr (\coef acc -> coef + x * acc) 0 poly 


-- | Using a random number generator it generates a number in Zp
genRandom :: DRG gen => gen       -- ^ A random number generator
                     -> (ZP, gen) -- ^ (A number in Zp, a new random number generator)
genRandom drg =  (\(n, drg') -> (fromIntegral n, drg')) $ withDRG drg (generateBetween 0 (toInteger $ p-1))

