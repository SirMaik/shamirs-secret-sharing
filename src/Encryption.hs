{-# LANGUAGE GADTs #-}

{-|
Module      : Encryption
Description : Encriptado / Desencriptado
Copyright   : -
License     : -
Maintainer  : mianorsi@ciencias.unam.mx
Stability   : experimental
Portability : POSIX

Aquí se encuentran las funcciones correspondientes a encriptar/desencriptar.
-}
module Encryption
  ( Key (Key),
    encrypt,
    decrypt
  ) where

import Data.ByteArray
import Crypto.Cipher.Types
import Crypto.Cipher.AES (AES256)
import Crypto.Error


-- | Tipo de dato algebraico que modela la llave para encriptar/desencriptar.
-- El tipo de dato c corresponde al algoritmo que se usará.
-- El tipo de dato a corresponde a la llave como tal.
data Key c a where
  Key :: (BlockCipher c, ByteArray a) => a -> Key c a

  
-- | Función que inicializa un cifrado de bloque
initCipher :: (BlockCipher c, ByteArray a) => Key c a               -- ^ La llave de encriptado/desencriptado.
                                           -> Either CryptoError c  -- ^ En el caso de ser exitoso (Right) devuelve una función de encriptado/desencriptado.
                                                                    -- ^ Si no devuelve un mensaje de error (Left).
initCipher (Key k) = case cipherInit k of
                       CryptoFailed e -> Left e
                       CryptoPassed a -> Right a


-- | Función que se usa para encriptar.  
encrypt :: (BlockCipher c, ByteArray a) => Key c a               -- ^ La llave de encriptado.
                                        -> a                     -- ^ El mensaje que se quiere encriptar
                                        -> Either CryptoError a  -- ^ El mensaje encriptado (Right) o un error (Left).
encrypt secretKey msg = case initCipher secretKey of
                          Left  e -> Left e
                          Right c -> Right $ ctrCombine c nullIV msg

-- | Función que se usa para desencriptar.
--   Es exactamente igual a la que se usa para encriptar. 
decrypt :: (BlockCipher c, ByteArray a) => Key c a               -- ^ La llave de encriptado.
                                        -> a                     -- ^ El mensaje que se quiere encriptar
                                        -> Either CryptoError a  -- ^ El mensaje encriptado (Right) o un error (Left).
decrypt = encrypt
