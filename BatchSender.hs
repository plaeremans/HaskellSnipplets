module Main where 

import System
import System.IO
import qualified Data.ByteString.Char8 as B
import Network


main = do 
  (fileName:_) <- getArgs
  cts <- B.readFile fileName
  let size = B.length cts
  handle <- connectTo "localhost" $ PortNumber 8899
  hPutStrLn handle "process"
  B.hPutStrLn handle (B.pack $ show size)
  B.hPut handle cts
  hClose handle
  return ()