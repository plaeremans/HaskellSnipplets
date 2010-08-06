module Main where

import System
import System.IO

import Data.List as L
import Data.Char
import Data.Set 

main = do
  args <- getArgs
  if ((length args) < 2) 
     then  putStrLn "You should provide two files" 
     else  
      do 
        putStrLn "ok"
        let arg1:arg2:_ = args 
        file1 <- openFile arg1 ReadMode
        file2 <- openFile arg2 ReadMode
        set1 <- fileToNumbers file1
        set2 <- fileToNumbers file2
        mapM (putStrLn . show)  (toList (difference set1 set2))
        hClose file1
        hClose file2
      where fileToNumbers file = readLines file >>= \x -> return $ fromList $ L.map  (read :: String -> Int) x

readLines :: Handle -> IO [String]
readLines handle = do
  contents <- hGetContents handle
  return $ words contents 
