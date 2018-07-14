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
    , replaceCellInRow
    , replaceCellFrom
    , replaceRowInGrid
    , replaceCellsInGrid
    , eliminateFromCell
    , eliminateFromRow
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
-- Randomness
    , genRandGrid
    , generateRandomValues
    , setRandomValue
    , randomNumberFromArray 
-- Utils
    , invalid
    , dmap
    , int2char 
    ) where

import Display
import System.Random

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

replaceCellInRow cell row = [ if cellCoordsMatch x cell then cell else x | x <- row ] 

replaceRowInGrid row grid = 
  let c = row !! 0
      (Cell (_,rowNum) _) = c
      (t,b) = splitAt (rowNum) grid
      newB = tail b
  in t ++ [row] ++ newB

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
-- advanceBookMark :: Num a => (a, b) -> (a, b)
-- advanceBookMark (8,y) = (0,y+1)
advanceBookMark (c,r) 
  | c == (gridSize - 1) = (0,r+1)
  | otherwise = (c+1, r)

                                  -- Querying
                                  -- --------

-- Querying individual cells

getCell :: (Int, Int) -> [[a]] -> a
getCell (x,y) grid = (grid !! (y)) !! (x)

getCellCoords :: Cell -> Coords
getCellCoords (Cell c _ ) = c

getCellRow :: Cell -> Int
getCellRow (Cell (_,r) _) = r

getCellCol :: Cell -> Int
getCellCol (Cell (c,_) _) = c

-- Get the resolved value of a cell (or -1 if all possibilities have been removed)
getCellValue (Cell _ v)
  | length v /= 1 = -1
  | otherwise = v !! 0 

-- Get the possible values for a cell
getCellPossibilities :: Cell -> [Int]
getCellPossibilities (Cell _ p) = p
 

getCol :: Int -> Grid Cell -> [Cell]
getCol c = map (!! c) 

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


-- replace cell with any in replacements that match the cell's coords
replaceCellFrom replacements cell = 
  let arrayWithNewCell = filter (cellCoordsMatch cell) replacements
  in if arrayWithNewCell == [] then cell else arrayWithNewCell !! 0 

  -- iterate over grid
  -- if any cell is included in cells
  -- return the cell from cells 
replaceCellsInGrid :: [Cell] -> Grid Cell -> Grid Cell
replaceCellsInGrid cells grid = dmap (replaceCellFrom cells) grid

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
      (Cell (colnum,rownum) poss ) = c
      val:_ = poss
      box = concat $ getBox c g2
      row = g2 !! rownum
      col = getCol colnum g2
      g3 = removeCellValueFromGridUsingArray c box g2
      g4 = removeCellValueFromGridUsingArray c row g3
      g5 = removeCellValueFromGridUsingArray c col g4
      in g5

genRandGrid :: StdGen -> Int -> (Grid Cell, Int)
genRandGrid gen iter = 
  let (grid,gen2) = generateRandomValues (mkGrid) (0,0) gen
  in if invalid grid then genRandGrid gen2 (iter+1) else (grid, iter)

                        -- *** *** *** UTILS *** *** ***

anyInValGrid = any . any
anyInGrid f grid = anyInValGrid f $ justVals grid
invalid  = anyInGrid (< 0) 

justCoords = dmap getCellCoords 
justVals = dmap getCellValue 
dmap = map . map


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

randomNumberFromArray :: StdGen -> [Int] -> (Int, StdGen)
randomNumberFromArray gen arr = 
  let l = length arr
      lastIndex = l - 1
      (i,g) = randomR (0,lastIndex) gen 
      n = arr !! i
  in (n,g)
