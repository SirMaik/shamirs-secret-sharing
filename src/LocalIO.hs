module LocalIO
    ( getPassword,
      printErr
    ) where

import System.IO as IO
import Control.Exception 
import qualified Data.ByteString as BS
--import Data.Text.IO as T.IO


--Recibe una cadena y la imprime en stderr. 
printErr :: String -> IO ()
printErr err = hPutStrLn stderr $ "Error: " ++ err

getPassword :: Int -> IO BS.ByteString
getPassword maxSize = do
  putStr "Password: "
  hFlush stdout
  pass <- withoutEcho BS.getLine
  putChar '\n'
  if BS.length pass > maxSize
    then (printErr . concat) ["Password can have a maximum of ", show maxSize, " characters."] >> getPassword maxSize
    else return pass

withoutEcho :: IO a -> IO a
withoutEcho action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin False) (hSetEcho stdin old) action


 
