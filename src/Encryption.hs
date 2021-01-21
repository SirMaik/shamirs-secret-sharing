{-# LANGUAGE GADTs #-}

module Encryption
  ( Key (Key),
    encrypt,
    decrypt
  ) where

import Data.ByteArray
import Crypto.Cipher.Types
import Crypto.Cipher.AES (AES256)
import Crypto.Error

-- | 
data Key c a where
  Key :: (BlockCipher c, ByteArray a) => a -> Key c a

  
-- Se inicializa un cifrador de bloque
initCipher :: (BlockCipher c, ByteArray a) => Key c a -> Either CryptoError c
initCipher (Key k) = case cipherInit k of
                       CryptoFailed e -> Left e
                       CryptoPassed a -> Right a


  
encrypt :: (BlockCipher c, ByteArray a) => Key c a -> a -> Either CryptoError a
encrypt secretKey msg = case initCipher secretKey of
                          Left  e -> Left e
                          Right c -> Right $ ctrCombine c nullIV msg

decrypt :: (BlockCipher c, ByteArray a) => Key c a -> a -> Either CryptoError a
decrypt = encrypt
