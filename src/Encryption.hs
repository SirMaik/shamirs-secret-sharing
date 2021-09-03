{-# LANGUAGE GADTs #-}

{-|
Module      : Encryption
Description : Encrypt/ Decrypt
Copyright   : -
License     : -
Maintainer  : mianorsi@ciencias.unam.mx
Stability   : experimental
Portability : POSIX

Here are the definitions that correspond to encrytion / decrytion.
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


-- | Algebraic data type which models a key used to encrypt/decrypt
-- Data type c corresponds to the algorithm that's going to be used
-- Data type a corresponds to the key
data Key c a where
  Key :: (BlockCipher c, ByteArray a) => a -> Key c a

  
-- | Function that initiates the block cypher
initCipher :: (BlockCipher c, ByteArray a) => Key c a               -- ^ The key to encrypt/decrypt
                                           -> Either CryptoError c  -- ^ Right -> returns an encryption/decryption function
                                                                    -- ^ Left  -> returns an error
initCipher (Key k) = case cipherInit k of
                       CryptoFailed e -> Left e
                       CryptoPassed a -> Right a


-- | Encryption function
encrypt :: (BlockCipher c, ByteArray a) => Key c a               -- ^ Encryption key
                                        -> a                     -- ^ The message to be encrypted
                                        -> Either CryptoError a  -- ^ Right -> returns the encrypted message
                                                                 -- ^ Left  -> returns an error
encrypt secretKey msg = case initCipher secretKey of
                          Left  e -> Left e
                          Right c -> Right $ ctrCombine c nullIV msg

-- | Decryption function
--   It's identical to the encryption function
decrypt :: (BlockCipher c, ByteArray a) => Key c a               -- ^ Decryption key
                                        -> a                     -- ^ The message to be decrypted
                                        -> Either CryptoError a  -- ^ Right -> returns the decrypted message
                                                                 -- ^ Left  -> returns an error
decrypt = encrypt
