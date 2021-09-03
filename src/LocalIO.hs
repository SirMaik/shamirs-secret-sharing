{-|
Module      : LocalIO
Description : Input / Output
Copyright   : -
License     : -
Maintainer  : mianorsi@ciencias.unam.mx
Stability   : experimental
Portability : POSIX

Here are the definitions that correspond to input / output.
-}
module LocalIO
    ( getPassword,
      printErr
    ) where

import System.IO as IO
import Control.Exception 
import qualified Data.ByteString as BS


-- |
printErr :: String -- ^ A string
         -> IO ()  -- ^ Prints the string to stderr
printErr err = hPutStrLn stderr $ "Error: " ++ err


-- | Function that is used to read a password from stdin
getPassword :: Int               -- ^ Maximum length of string

            -> IO BS.ByteString  -- ^ Password
getPassword maxSize = do
  putStr "Password: "
  hFlush stdout
  pass <- withoutEcho BS.getLine
  putChar '\n'
  if BS.length pass > maxSize
    then (printErr . concat) ["Password can have a maximum of ", show maxSize, " characters."] >> getPassword maxSize
    else return pass


-- | Function that carries out an IO action without echo in the terminal
withoutEcho :: IO a -- ^ IO action to be carried out
            -> IO a -- ^ Result of the action
withoutEcho action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin False) (hSetEcho stdin old) action


 
