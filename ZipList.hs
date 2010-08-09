module Main where

import System
import System.IO

import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Set 
import Codec.Archive.Zip

main = do
  args <- getArgs
  if ((length args) < 1) 
     then  putStrLn "You should provide a  file as argument" 
     else  
      do 
        let arg1:_ = args 
        bytes <- B.readFile arg1
	let ar = toArchive bytes
	let cdc = eRelativePath $ head $ zEntries ar	
	putStrLn $ show cdc
        return ()
