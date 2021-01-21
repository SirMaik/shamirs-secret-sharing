module Polynomial
    (randPoly,
     randPoints,
     lagrange,
     horner
    ) where

import Data.List
import Crypto.Number.Generate
import Crypto.Random
  

import Zp 


type X = ZP
type Y = ZP
type Point = (X, Y)


--A finite polynomial [c, x, x^2, x^3, ...]
type Degree      = Integer
type Coefficient = ZP
type Polynomial = [Coefficient]


degree :: Polynomial -> Degree
degree = genericLength


hasDuplicates :: Ord a => [a] -> Bool
hasDuplicates xs = f (sort xs) where
  f (x : t@(y : _)) = x == y || f t
  f _ = False

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing




randPoly :: (DRG gen) => gen -> ZP -> Degree -> (Polynomial, gen)
randPoly drg const deg =  let l    = genericTake deg $ iterate (genRandom . snd) (genRandom drg)
                              drg' = (snd . last) l
                              poly =  map fst l
                          in if hasDuplicates poly
                             then randPoly drg' const deg
                             else (const:poly, drg')

                              
randPoints :: (DRG gen) => gen  -> Polynomial -> Integer -> ([Point], gen)
randPoints drg poly n = let l    = genericTake n $ iterate (genRandom . snd) (genRandom drg)
                            drg' = (snd . last) l
                            xs   =  map fst l 
                        in if hasDuplicates xs
                           then randPoints drg' poly n
                           else (map (\x -> (x, horner poly x)) xs, drg')
                             

--Método de interpolación de lagrange
--Se puede dividir para no cargar tuplas
lagrange :: [Point] -> X -> Y
lagrange poly x = sum $ map (\p1@(x1,y1) -> (\r -> y1 * r) (basePol p1)) poly
  where basePol pi@(xi,_) = foldr (\p2@(x2,_) r -> if pi /= p2 then ((x-x2)/(xi-x2))*r else r) 1 poly

  
--Algoritmo de evaluación de Horner
horner :: Polynomial -> X -> Y
horner poly x = foldr (\coef acc -> coef + x * acc) 0 poly 
 
genRandom :: DRG gen => gen -> (ZP, gen) 
genRandom drg =  (\(n, drg') -> (fromIntegral n, drg')) $ withDRG drg (generateBetween 0 (toInteger $ p-1))

