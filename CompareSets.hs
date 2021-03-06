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
        let arg1:arg2:_ = args 
        set1 <- fileToNumbers arg1
        set2 <- fileToNumbers arg2
        mapM (putStrLn . show)  (toList (difference set1 set2))
        return ()
      where 
        fileToNumbers file = readFile file >>= return . fromList .  (L.map  (read :: String -> Int)) .  words 
