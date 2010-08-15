module Main where 

import System
import System.IO
import qualified Data.ByteString.Char8 as B
import Network


main = do 
  (command:rest) <- getArgs
  handleCommand command rest

connect = connectTo "localhost" $ PortNumber 8899

handleCommand "send" args = do
  let fileName:_ = args
  cts <- B.readFile fileName
  let size = B.length cts
  handle <- connect
  hPutStrLn handle "process"
  B.hPutStrLn handle (B.pack $ show size)
  B.hPut handle cts
  flushAndRead handle

handleCommand "fetch" args = do
  let ticket:_ = args
  handle <- connect
  hPutStrLn handle "fetchResult" 
  hPutStrLn handle ticket
  flushAndRead handle
handleCommand _ _  = return ()

flushAndRead handle = do 
  hFlush handle
  copy handle
  hClose handle
  return ()

copy handle = do
  eof <- hIsEOF handle
  if eof 
     then do
       return ()
     else
         do
           line <- hGetLine handle
           handleLine line handle


handleLine line handle = if null line then (term) else (ctu)
    where term = hClose handle 
          ctu = do
            putStrLn line
            copy handle