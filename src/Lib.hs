module Lib
    ( filteredEliminateFromCell
    , invalid
    , cellCoordsMatch
    , getNewVersion
    , getBox
    , getBoxRows
    , getBoxCoord
    , getBoxCoords 
    , eliminateFromRow
    , eliminateFromCell
    , int2char 
    , justVals 
    , justCoords 
    , dmap
    , getCell 
    , getCellCoords 
    , getCellValue
    , generateSmallRandomValues 
    , advanceBookMark 
    , randomNumber
    , newCell
    , Cell(Cell)
    , mkGrid
    , replaceCellInRow
    , replaceCellsInGrid
    , replaceRowInGrid
    , genRandGrid
    , generateRandomValues
    , setRandomValue
    , makeSmallerGrid 
    , setValue
    , randomNumberFromArray 
    , chooseCellInGrid
    ) where

import Display
import System.Random

randomNumber :: StdGen -> (Int, StdGen)
randomNumber = randomR (1,9)

randomNumberFromArray :: StdGen -> [Int] -> (Int, StdGen)
randomNumberFromArray gen arr = 
  let l = length arr
      lastIndex = l - 1
      (i,g) = randomR (0,lastIndex) gen 
      n = arr !! i
  in (n,g)


makeRandomRow _ 0 _ = []
makeRandomRow gen countOfNumsToFind chosenNums = 
  let (newNum,newGen) = randomNumber gen
  in if chosenNums `contains` newNum
    then ( makeRandomRow newGen countOfNumsToFind chosenNums) -- try again
    else newNum : makeRandomRow newGen (countOfNumsToFind-1) (newNum : chosenNums) -- add new number to found items

-- int2char a = (show a) !! 0

-- showRandomGrid gen = displayNumberGrid $ map (map int2char) (makeRandomGrid gen)
-- makeRandomGrid gen = replicate 9 $ makeRandomRow gen 9 []

-- coords then array of possible values
data Cell = 
  Cell (Int, Int) [Int] 
  deriving (Eq, Ord, Show)

type Grid a = [[a]]
type Coords = (Int,Int)

newCell :: Coords -> Cell
newCell c = Cell c [1..9]


newCellFromCount count num = newCell ((9 - count), (9 - num))

mkGrid = makeCellRows 9

-- showCellGrid = putStrLn $ concat $ map (\x -> (show x) ++"\n") mkGrid
-- showRandCellGrid gen = putStrLn $ concat $ map (\x -> (show x) ++ "\n") (genRandGrid gen)

makeCellRow :: Int -> Int -> [Cell]
makeCellRow 0 _ = []
makeCellRow count num = ( newCellFromCount count num) : makeCellRow (count-1) num

makeCellRows :: Int -> Grid Cell
makeCellRows 0 = []
makeCellRows count = makeCellRow 9 count : makeCellRows (count-1)

-- *** *** *** Small *** *** *** 

newSmallCellFromCount count num = newSmallCell ((4 - count), (4 - num))

newSmallCell :: Coords -> Cell
newSmallCell c = Cell c [1..3]

makeSmallerGrid = makeSmallCellRows 3

makeSmallCellRow :: Int -> Int -> [Cell]
makeSmallCellRow 0 _ = []
makeSmallCellRow count num = ( newSmallCellFromCount count num) : makeSmallCellRow (count-1) num

makeSmallCellRows :: Int -> Grid Cell
makeSmallCellRows 0 = []
makeSmallCellRows count = makeSmallCellRow 3 count : makeSmallCellRows (count-1)

generateSmallRandomValues :: Grid Cell -> Coords -> StdGen -> Grid Cell
generateSmallRandomValues grid (_,4) gen = grid
generateSmallRandomValues grid (4,y) gen = generateSmallRandomValues grid (1,y+1) gen
generateSmallRandomValues grid bookmark gen = 
  let cellToChange = getCell bookmark grid
      (changedCell, ng) = setRandomValue gen cellToChange
      (_,rowNum) = bookmark
      row = grid !! (rowNum - 1)
      changedRow = replaceCellInRow changedCell row 
      newGrid = replaceRowInGrid changedRow grid
      newBookMark = advanceBookMark bookmark
  in generateSmallRandomValues newGrid newBookMark ng
 
-- -- -- -- -- 

replaceCellInRow cell row = 
  let (Cell (x,_) values) = cell
      (f,l) = splitAt (x) row
      newL = tail l
  in f ++ [cell] ++ newL

replaceRowInGrid row grid = 
  let c = row !! 0
      (Cell (_,rowNum) _) = c
      (t,b) = splitAt (rowNum) grid
      newB = tail b
  in t ++ [row] ++ newB

getCell :: (Int, Int) -> [[a]] -> a
getCell (x,y) grid = (grid !! (y)) !! (x)

setValue :: Int -> Cell -> Cell
setValue a cell = 
  let (Cell x list) = cell
  in Cell x [a]

-- Get the possible values for a cell
getPoss :: Cell -> [Int]
getPoss (Cell _ p) = p
 
-- Choose one of the possible values as the cells values
setRandomValue :: StdGen -> Cell -> (Cell , StdGen)
setRandomValue gen cell = 
  let poss = getPoss cell
      (n,g) = randomNumberFromArray gen poss
  in (setValue n cell,g)  -- todo replace with random

-- move the bookmark one cell forward
-- advanceBookMark :: Num a => (a, b) -> (a, b)
advanceBookMark (8,y) = (0,y+1)
advanceBookMark bookmark =
  let (c,r) = bookmark
  in (c+1, r)

generateRandomValues :: Grid Cell -> Coords -> StdGen -> (Grid Cell, StdGen)
generateRandomValues grid (_,9) gen = (grid, gen)
generateRandomValues grid bookmark gen = 
  let cellToChange = getCell bookmark grid
      (changedCell,ng) = setRandomValue gen cellToChange
      newGrid = chooseCellInGrid changedCell grid
      newBookMark = advanceBookMark bookmark
  in  generateRandomValues newGrid newBookMark ng
 

getBoxRows boxCoord = take 3 . drop (boxCoord * 3) 
getBoxCols boxCoord = map $ take 3 . drop (boxCoord * 3)

getBoxCoord a = div a 3

getBoxCoords :: Coords -> Coords
getBoxCoords (col,row) = 
  let boxCol = getBoxCoord col 
      boxRow = getBoxCoord row 
  in (boxCol,boxRow)

-- Returns the box that the cell is in
getBox :: Cell -> Grid Cell -> Grid Cell
getBox c g = 
  let (Cell (col,row) _ ) = c
      (bc,br) = getBoxCoords (col,row)
  in getBoxCols bc $ getBoxRows br g

cellCoordsMatch :: Cell -> Cell -> Bool
cellCoordsMatch a b = 
  getCellCoords a == getCellCoords b

rowXOrColMatch:: Cell -> Cell -> Bool
rowXOrColMatch a b = 
  let (ac,ar) = getCellCoords a
      (bc,br) = getCellCoords b
  in (ac == bc) /=  (ar == br)

getCol :: Int -> Grid Cell -> [Cell]
getCol c = map (!! c) 

getNewVersion cells cell = 
  let arrayWithNewCell = filter (cellCoordsMatch cell) cells
  in if arrayWithNewCell == [] then cell else arrayWithNewCell !! 0 

replaceCellsInGrid :: [Cell] -> Grid Cell -> Grid Cell
replaceCellsInGrid cells grid = dmap (getNewVersion cells) grid
  -- iterate over grid
  -- if any cell is included in cells
  -- return the cell from cells 



chooseCellInGrid :: Cell -> Grid Cell -> Grid Cell
chooseCellInGrid c g = 
  let g2 = replaceCellsInGrid [c] g
      (Cell (col,row) poss ) = c
      val:_ = poss
      box = getBox c g2
      flatBox = concat box
      newFlatBox = map (filteredEliminateFromCell val c) flatBox 
      g3 = replaceCellsInGrid newFlatBox g2
      gridRow = g3 !! (row)
      newRow = map (filteredEliminateFromCell val c) gridRow
      g4 = replaceCellsInGrid newRow g3
      gridCol = getCol (col) g4
      newCol = map (filteredEliminateFromCell val c) gridCol
      g5 = replaceCellsInGrid newCol g4
-- TODO
  in g5

genRandGrid gen = 
  let (grid,gen2) = generateRandomValues (mkGrid) (0,0) gen
  in if invalid grid then genRandGrid gen2 else grid

-- *** *** *** UTILS *** *** ***

anyInValGrid = any . any
anyInGrid f grid = anyInValGrid f $ justVals grid
invalid  = anyInGrid (< 0) 

justCoords = dmap getCellCoords 
justVals = dmap getCellValue 
dmap = map . map
contains = flip elem
getCellValue (Cell _ v)
  | length v /= 1 = -1
  | otherwise = v !! 0 

getCellCoords (Cell c _ ) = c

int2char :: Int -> Char
int2char a = (show a) !! 0

eliminateFromRow :: Int -> [Cell] -> [Cell]
eliminateFromRow = map . eliminateFromCell

filteredEliminateFromCell :: Int -> Cell -> Cell -> Cell
filteredEliminateFromCell v newC oldC = 
  if cellCoordsMatch newC oldC 
  then oldC 
  else eliminateFromCell v oldC


eliminateFromCell :: Int ->  Cell -> Cell
eliminateFromCell n (Cell c v) = (Cell c $ eliminateFromArray n v)

eliminateFromArray :: Int -> [Int] -> [Int]
eliminateFromArray n arr = 
  let filteredArray = filter (\x -> x /= n) arr
  in if filteredArray == [] then [-5] else filteredArray
