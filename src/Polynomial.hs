{-|
Module      : Polynomial
Description : Manejo de poliniomios
Copyright   : -
License     : -
Maintainer  : mianorsi@ciencias.unam.mx
Stability   : experimental
Portability : POSIX

En este módulo se define aquello correspondiente al manejo de polinomios.
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

-- | Coordenada x.
type X = ZP
-- | Coordenada y.
type Y = ZP
-- | Un punto (x,y).
type Point = (X, Y)


-- | El crado de un polinomio.
type Degree = Integer
-- | Los coeficientes del polinomio se encuentran en Zp.
type Coefficient = ZP
-- | El polinomio se modela como una lista de coeficientes.
-- Si el polinomio es a*x^n + ... + b*x + c entonces la lista correspondiente es [c, b, ..., a]
type Polynomial = [Coefficient]

-- | Función que devuelve el grado del polinomio.
degree :: Polynomial -- ^ El polinomio.
       -> Degree     -- ^ El grado del polinomio. 
degree = genericLength

-- | Función auxiliar que revisa si una lista tiene duplicados.
-- Servirá cuando queramos generar listas aleatorias con elementos únicos
hasDuplicates :: Ord a => [a]  -- ^ La lista
                       -> Bool -- ^ Si la lista tiene duplicados o no.
hasDuplicates xs = f (sort xs) where
  f (x : t@(y : _)) = x == y || f t
  f _ = False


-- | Función que genera un polinomio aleatorio sin repeticiones de coeficientes.
randPoly :: (DRG gen) => gen               -- ^ Generador de números aleatorios.
                      -> ZP                -- ^ El término constante del polinomio.
                      -> Degree            -- ^ El grado del polinomio
                      -> (Polynomial, gen) -- ^ Un polinomio aleatorio y un nuevo generador de números alteatorios.
randPoly drg const deg =  let l    = genericTake deg $ iterate (genRandom . snd) (genRandom drg)
                              drg' = (snd . last) l
                              poly =  map fst l
                          in if hasDuplicates poly
                             then randPoly drg' const deg
                             else (const:poly, drg')

-- | Función que genera una lista aleatoria sin repeticiones de puntos correspondientes a cierto polinomio.
randPoints :: (DRG gen) => gen            -- ^ Generador de números aleatorios.
                        -> Polynomial     -- ^ El polinomio .
                        -> Integer        -- ^ El número de puntos.
                        -> ([Point], gen) -- ^ La lista de puntos aleatorios y un nuevo generador de números aleatorios.
randPoints drg poly n = let l    = genericTake n $ iterate (genRandom . snd) (genRandom drg)
                            drg' = (snd . last) l
                            xs   =  map fst l 
                        in if hasDuplicates xs
                           then randPoints drg' poly n
                           else (map (\x -> (x, horner poly x)) xs, drg')
                             

-- | Método de interpolación de Lagrange.
-- Toma una lista de puntos y a partir de ella y un valor evalúa el polinomio correspondiente.
lagrange :: [Point] -- ^ La lista de puntos.
         -> X       -- ^ El valor que se quiere evaluar.
         -> Y       -- ^ El resultado.
lagrange poly x = sum $ map (\p1@(x1,y1) -> (\r -> y1 * r) (basePol p1)) poly
  where basePol pi@(xi,_) = foldr (\p2@(x2,_) r -> if pi /= p2 then ((x-x2)/(xi-x2))*r else r) 1 poly

  
-- | Algoritmo de evaluación de Horner.
-- Toma un polinomio y un valor y devuelve la evaluación del polinomio con ese valor
horner :: Polynomial -- ^ El polinomio.
       -> X          -- ^ El valor que se quiere evaluar.
       -> Y          -- ^ El resultado.-> X -> Y
horner poly x = foldr (\coef acc -> coef + x * acc) 0 poly 


-- | A partir de un generador de números aleatorios genera un número en Zp
--   y otro generador de números aleatorios.
genRandom :: DRG gen => gen       -- ^ El generador de números aleatorios.
                     -> (ZP, gen) -- ^ Un número aleatorio y otro generador.
genRandom drg =  (\(n, drg') -> (fromIntegral n, drg')) $ withDRG drg (generateBetween 0 (toInteger $ p-1))

