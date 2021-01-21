module Main where

import Numeric
import System.Environment   
import System.IO as IO
import Data.Maybe
import Data.Char
import Data.Either
import Data.ByteString  as BS (ByteString, readFile, writeFile, length)
import Data.ByteArray (convert, empty)
import Data.String (fromString)
import Control.Monad
import Crypto.Hash
import Crypto.Random
import Crypto.Cipher.Types
import Crypto.Cipher.AES (AES256)

import LocalIO
import Polynomial
import Encryption 
import Misc

data Mode = Encrypt Integer Integer FilePath FilePath FilePath
          | Decrypt FilePath FilePath FilePath


parse :: [String] -> Either String (Mode -> IO (), Mode)
parse ["c", n, t, plain, pts, ciph]  = if all isDigit n && all isDigit t
                                 then let n' = read n
                                          t' = read t
                                      in if n' > 2 && t' > 1 && t' <= n'
                                         then Right (encryptMode, Encrypt n' t' plain pts ciph)
                                         else Left "The following must be satisfied: 2 < n ^ 1 < t ^ t <= n."
                                 else Left "\"n\" and \"t\" must be integers."
parse ["d", points, ciph, plain] = Right (decryptMode, Decrypt points ciph plain)
parse _                          = Left "Invalid argument combination." --Se puede desglosar en mensajes más claros
                                     
       

encryptMode :: Mode -> IO ()
encryptMode (Encrypt n t plain points ciph) = do
  pass      <- getPassword 30
  plainFile <- BS.readFile plain
  let hash = hashWith SHA256 pass
      key  = (Key . convert) hash :: Key AES256 ByteString
  case encrypt key plainFile of
    Left  e -> (printErr . show) e
    Right c -> do drg    <- getSystemDRG                  
                  let [(const,_)]  = (readHex . show) hash
                      (poly, drg') = randPoly   drg  (fromInteger const) (t-1)
                      (pts , _   ) = randPoints drg' poly n            
                  BS.writeFile ciph c                  
                  ptsHandle <- openFile points WriteMode 
                  mapM_ (\p -> hPutStrLn ptsHandle (show p)) pts
                  hClose ptsHandle

                    
decryptMode :: Mode -> IO ()
decryptMode (Decrypt points ciph plain) = do
  ptsFile  <- IO.readFile points
  ciphFile <- BS.readFile ciph
  let pts   = ptsFromString ptsFile
      const = lagrange pts (fromIntegral 0)
      key   = (Key . toByteString) const :: Key AES256 ByteString
  case decrypt key ciphFile of
    Left  e -> (printErr . show) e
    Right c -> BS.writeFile plain c


main :: IO ()
main = do 
  args <- getArgs
  case parse args of
    Right (action, mode) -> action mode
    Left  err            -> printErr err

    
  
  -- print args
    

