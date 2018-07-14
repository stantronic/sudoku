module Main where

import Lib
import System.IO
import System.Random
import Display

main :: IO ()
main = do
  gen <- newStdGen
  -- hSetBuffering stdout NoBuffering
  displayNumberGrid $ dmap int2char $ justVals $ genRandGrid gen 
exit a = a == "q"


---- playTurn :: a -> IO()
--  putStrLn $ "Welcome"
--  putStr "Please enter a number > "
--  number <- getLine
--  let (n,newGen) = randomNumber gen
--  putStrLn $ "Boring. My random number is " ++ (show n)
--  putStrLn "Look at my nice grid:"
--  -- showRandCellGrid newGen
----  showRandomGrid newGen
--  -- putStrLn $ makeRandomGrid newGen
---- $ randomNumber gen)
--  if exit number then
--    putStrLn "Bye!"
--  else
--    playTurn newGen

-- playTurn gen = do
