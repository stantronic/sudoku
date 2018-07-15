module Lib
    ( 
-- Types
      Cell(Cell)
-- Constants
    , gridSize
-- Creation
    , mkGrid
    , newCell
-- Transformation
    , setValue
    , chooseCellInGrid
    , filteredEliminateFromCell
    , advanceBookMark 
    , replaceCellFrom
    , replaceCellsInGrid
    , eliminateFromCell
-- Retrieval
    , justVals 
    , justCoords 
    , getBox
    , getBoxRows
    , getCell 
    , getCellCoords 
    , getBoxCoord
    , getBoxCoords 
    , getCellValue
-- Query
    , cellCoordsMatch
    , invalid
-- Randomness
    , genRandGrid
    , generateRandomValues
    , setRandomValue
    , randomNumberFromArray 
-- Utils
    , dmap
    -- , int2char 
    , concatFunc
    ) where

import System.Random
import Data.Maybe
                                    -- Types
                                    -- -----

-- A Cell consists of coords then array of possible values
data Cell = 
  Cell (Int, Int) [Int] 
  deriving (Eq, Ord, Show)

-- A grid is simply a two dimensional array
type Grid a = [[a]]

-- Coords are a tuple - (column, row) beginning at 0,0 in the top left
type Coords = (Int,Int)

-- Constants
gridSize = 9
boxesPerRow = 3
boxSize = div gridSize boxesPerRow

                                  -- Creation
                                  -- --------

-- A cell is initialised with its coords, and starts with all possible values
newCell :: Coords -> Cell
newCell c = Cell c [1..gridSize]

newCellAtCoords x y = newCell (x,y)

makeCellRow :: Int -> Int -> [Cell]
makeCellRow 0 _ = []
makeCellRow count num = newCellAtCoords (gridSize - count) num : makeCellRow (count-1) num

makeCellRows :: Int -> Grid Cell
makeCellRows 0 = []
makeCellRows count = makeCellRow gridSize (gridSize - count) : makeCellRows (count-1)

mkGrid = makeCellRows gridSize

                               -- Transformation
                               -- --------------

setValue :: Int -> Cell -> Cell
setValue a cell = 
  let (Cell x list) = cell
  in Cell x [a]

-- Choose one of the possible values as the cells values
setRandomValue :: StdGen -> Cell -> (Cell , StdGen)
setRandomValue gen cell = 
  let poss = getCellPossibilities cell
      (n,g2) = randomNumberFromArray gen poss
  in (setValue n cell,g2) 

-- move the bookmark one cell forward
advanceBookMark :: Coords -> Coords
advanceBookMark (c,r) 
  | c == (gridSize - 1) = (0,r+1)
  | otherwise = (c+1, r)

                                  -- Retrieval
                                  -- --------

-- Retrieving values from individual cells

getCell :: (Int, Int) -> [[a]] -> a
getCell (x,y) grid = (grid !! (y)) !! (x)

getCellCoords :: Cell -> Coords
getCellCoords (Cell c _ ) = c

getCellRow :: Cell -> Int
getCellRow (Cell (_,r) _) = r

getCellCol :: Cell -> Int
getCellCol (Cell (c,_) _) = c

-- Get the resolved value of a cell (or -10 if all possibilities have been removed)
getCellValue (Cell _ v)
  | length v /= 1 = -10
  | otherwise = v !! 0 

-- Get the possible values for a cell
getCellPossibilities :: Cell -> [Int]
getCellPossibilities (Cell _ p) = p

-- Retrieving values from the whole grid
getCol :: Int -> Grid Cell -> [Cell]
getCol c = map (!! c) 

getRow :: Int -> Grid Cell -> [Cell]
getRow c g = g !! c

getBoxRows boxCoord = take boxSize . drop (boxCoord * boxSize) 
getBoxCols boxCoord = map $ take boxSize . drop (boxCoord * boxSize)

getBoxCoord a = div a boxSize

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

-- Randomise

generateRandomValues :: Grid Cell -> Coords -> StdGen -> (Grid Cell, StdGen)
generateRandomValues grid (_,9) gen = (grid, gen)
generateRandomValues grid bookmark gen = 
  let cellToChange = getCell bookmark grid
      (changedCell,ng) = setRandomValue gen cellToChange
      newGrid = chooseCellInGrid changedCell grid
      newBookMark = advanceBookMark bookmark
  in  generateRandomValues newGrid newBookMark ng
 
                                 -- Validation
                                 -- ----------

cellCoordsMatch :: Cell -> Cell -> Bool
cellCoordsMatch a b = 
  getCellCoords a == getCellCoords b

rowXOrColMatch :: Cell -> Cell -> Bool
rowXOrColMatch a b = 
  let (ac,ar) = getCellCoords a
      (bc,br) = getCellCoords b
  in (ac == bc) /=  (ar == br)

cellHasOneValue :: Cell -> Bool
cellHasOneValue (Cell c v) = length v == 1

-- returns a tuple of 
  -- a) the cell, or the cell's replacement if one exists in replacements
  -- b) a maybe cell, which will contain the replacement if making the replacement narrows its possibilties to 1 value
replaceCellFrom :: [Cell] -> Cell -> (Cell, Maybe Cell)
replaceCellFrom replacements cell = 
  let arrayWithNewCell = filter (cellCoordsMatch cell) replacements
  in if arrayWithNewCell == [] then (cell, Nothing) 
  else 
    let newCell = head arrayWithNewCell 
        maybeCell = if cellHasOneValue newCell && not (cellHasOneValue cell) then Just newCell else Nothing
    in (newCell, maybeCell)


-- Given a list of cells return a grid with those cells inserted
-- If any replacement results in reducing its possibilities to one, then choose that cell in the grid
replaceCellsInGrid :: [Cell] -> Grid Cell -> Grid Cell
replaceCellsInGrid newCells grid = 
  let newGridWithOneValues = dmap (replaceCellFrom newCells) grid
      getCellOnly (c, _) = c
      getOneValueCells (_, x) = x
      newGrid = dmap getCellOnly newGridWithOneValues  
      maybes = catMaybes $ concat $ dmap getOneValueCells newGridWithOneValues
  in if length maybes > 0 then chooseCellInGrid (maybes !! 0) newGrid else newGrid

removeCellValueFromCellArray :: Cell -> [Cell] -> [Cell]
removeCellValueFromCellArray c arr = 
  let val = getCellValue c
  in map (filteredEliminateFromCell val c) arr

removeCellValueFromGridUsingArray :: Cell -> [Cell] -> Grid Cell -> Grid Cell
removeCellValueFromGridUsingArray c arr grid = 
  let processedCells = removeCellValueFromCellArray c arr
  in replaceCellsInGrid processedCells grid

chooseCellInGrid :: Cell -> Grid Cell -> Grid Cell
chooseCellInGrid c g = 
  let g2 = replaceCellsInGrid [c] g
      (Cell (colnum,rownum) _ ) = c
      box = concat $ getBox c g2
      row = getRow rownum g2
      col = getCol colnum g2
      eliminatePossibilities = concatFunc $ map (removeCellValueFromGridUsingArray c) [box, row, col] 
      in eliminatePossibilities g2

genRandGrid :: StdGen -> Int -> (Grid Cell, Int)
genRandGrid gen iter = 
  let (grid,gen2) = generateRandomValues (mkGrid) (0,0) gen
  in if invalid grid then genRandGrid gen2 (iter+1) else (grid, iter + 1)

                        -- *** *** *** UTILS *** *** ***

concatFunc (a:[]) = a
concatFunc (a:as) = (concatFunc as) . a

anyInValGrid = any . any
anyInGrid f grid = anyInValGrid f $ justVals grid
invalid = anyInGrid (< 0) 

justCoords = dmap getCellCoords 
justVals = dmap getCellValue 
dmap = map . map

-- int2char :: Int -> Char
-- int2char a = (show a) !! 0

-- Will eliminate the newCells value from the oldCell unless the coords match
filteredEliminateFromCell :: Int -> Cell -> Cell -> Cell
filteredEliminateFromCell v newC oldC = 
  if cellCoordsMatch newC oldC 
  then oldC 
  else eliminateFromCell  newC oldC

-- eliminate the first cell's value from the second cell's possibilities
eliminateFromCell :: Cell ->  Cell -> Cell
eliminateFromCell (Cell _ v1) (Cell c2 v2) = (Cell c2 $ eliminateFromArray (head v1) v2)

eliminateFromArray :: Int -> [Int] -> [Int]
eliminateFromArray n arr = 
  let filteredArray = filter (\x -> x /= n) arr
  in if filteredArray == [] then [negate n] else filteredArray

randomNumberFromArray :: StdGen -> [Int] -> (Int, StdGen)
randomNumberFromArray gen arr = 
  let l = length arr
      lastIndex = l - 1
      (i,g) = randomR (0,lastIndex) gen 
      n = arr !! i
  in (n,g)
