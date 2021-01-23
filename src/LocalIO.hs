{-|
Module      : LocalIO
Description : Entrada / Salida
Copyright   : -
License     : -
Maintainer  : mianorsi@ciencias.unam.mx
Stability   : experimental
Portability : POSIX

Aquí se encuentran las funciones correspondientes a la entrada / salida.
-}
module LocalIO
    ( getPassword,
      printErr
    ) where

import System.IO as IO
import Control.Exception 
import qualified Data.ByteString as BS


-- | Recibe una cadena y la imprime en stderr.
printErr :: String -> IO ()
printErr err = hPutStrLn stderr $ "Error: " ++ err


-- | Función que se usa para leer una contraseña de la entrada estándar.
getPassword :: Int               -- ^ Longitud máxima de la contraseña.
            -> IO BS.ByteString  -- ^ Contraseña leída. 
getPassword maxSize = do
  putStr "Password: "
  hFlush stdout
  pass <- withoutEcho BS.getLine
  putChar '\n'
  if BS.length pass > maxSize
    then (printErr . concat) ["Password can have a maximum of ", show maxSize, " characters."] >> getPassword maxSize
    else return pass


-- | Función que lleva a cabo una acción sin eco.
withoutEcho :: IO a -- ^ Acción que se quiere llevar a cabo
            -> IO a -- ^ Resultado de la acción. 
withoutEcho action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin False) (hSetEcho stdin old) action


 
