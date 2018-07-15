module Main where

import Lib
import System.IO
import System.Random
import Display

main :: IO ()
main = do
  gen <- newStdGen
  -- hSetBuffering stdout NoBuffering
  let (grid,iter) = genRandGrid gen 0
  do
    displayCellGrid grid
    -- displayNumberGrid $ dmap int2char $ justVals grid
    putStrLn $ "Iterations = " ++ (show iter)

-- exit a = a == "q"

---- playTurn :: a -> IO()
--  putStrLn $ "Welcome"
--  putStr "Please enter a number > "
--  number <- getLine
--  if exit number then
--    putStrLn "Bye!"
--  else
--    playTurn newGen

