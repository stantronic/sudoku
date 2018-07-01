module Lib
    ( someFunc
    ) where

import Data.Char
import Data.List

numbers = [1..9]

separator = "\n+---+---+---+---+---+---+---+---+---+\n"

someFunc :: IO ()
someFunc = putStrLn $ separator ++ allLines

allLines = concat $ map generateLineFrom numbers


generateLine :: [Char]
generateLine = concat $ map makeCell numbers

generateLineFrom :: Int -> [Char]
generateLineFrom _ = generateLine ++ "|" ++ separator

makeCell :: Int -> [Char]
makeCell a = "| " ++ show a ++ " "
