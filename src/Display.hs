module Display
      ( boxEdgeH
      , gridSideEdge
      , gridSpaceRow
      , emptyGrid
      , displayEmptyGrid
      , fakeNumberGrid
      , makeNumberRowSegment
      , fakeCharGrid
      , displayNumberGrid 
      ) where

import Data.Char
import Data.List
import Data.String.Utils(replace)

-- terminology:

--	Grid: the whole sudoku puzzle made up of 9 Boxes
--  Box: a 3 cell x 3 cell square
--	Cell: is either blank or contains 1 digit
--  Edges are between Boxes
--  Corners are where Edges join
--  Guides are between Cells
--  GuideCorners are where Guides join

cellWidth = 5

corner = green ++ "+"
guideCorner = yellow ++ "+"
verticalGuideSegment = yellow ++ ":"
verticalEdgeSegment = green ++ "|" 

black = "\ESC[30m"
red = "\ESC[31m"
green = "\ESC[32m" 
yellow = "\ESC[33m"
blue = "\ESC[34m"
magenta = "\ESC[35m"
cyan = "\ESC[36m"
white = "\ESC[37m"


int2char a = (show a) !! 0
fakeNumberGrid = replicate 9 [1..9]
fakeCharGrid = map (map int2char) fakeNumberGrid

formattedFakeNumberGrid = concat $ map appendNewLine fakeCharGrid 


-- actually puts it on the screen
displayEmptyGrid = putStrLn $ emptyGrid(fakeCharGrid)
displayNumberGrid grid = putStrLn $ emptyGrid grid

-- Full grid display as string
emptyGrid g = 
  let top = take 3 g
      mid = take 3 $ drop 3 g
      bot = take 3 $ drop 6 g
  in mergeBoxEdges $ (boxRow top)++ (boxRow mid) ++ (boxRow bot)

{-|

replace "replace" "pave" "please replace this with a replacement" 

Prints:

+-----+-----+-----+-----+-----+-----+-----+-----+-----+
|     :     :     |     :     :     |     :     :     |
|  2  :  1  :  0  |  2  :  1  :  0  |  2  :  1  :  0  |
+.....+.....+.....+.....+.....+.....+.....+.....+.....+
|     :     :     |     :     :     |     :     :     |
|  2  :  1  :  0  |  2  :  1  :  0  |  2  :  1  :  0  |
+.....+.....+.....+.....+.....+.....+.....+.....+.....+
|     :     :     |     :     :     |     :     :     |
|  2  :  1  :  0  |  2  :  1  :  0  |  2  :  1  :  0  |
+-----+-----+-----+-----+-----+-----+-----+-----+-----+

-}

addEdges a = green ++ gridSideEdge ++ a ++ "\n" ++ green ++ gridSideEdge
boxRow = addEdges . rawGrid 

rawGrid x = "\n" ++ gridSpaceOnlyRow
  ++ "\n" ++ gridNumberRow (x !! 0) 
  ++ "\n" ++ gridHGuide
  ++ "\n" ++ gridSpaceOnlyRow
  ++ "\n" ++ gridNumberRow (x !! 1)
  ++ "\n" ++ gridHGuide
  ++ "\n" ++ gridSpaceOnlyRow
  ++ "\n" ++ gridNumberRow (x !! 2)

textTimes a 1 = a
textTimes a b = a ++ textTimes a (b-1)

cellGuideH = yellow ++ (replicate cellWidth '.')
cellEdgeH = green ++ (replicate cellWidth '-')
cellRowSpace = replicate cellWidth ' '

makeEdgeSegmentH :: Int -> [Char]
makeEdgeSegmentH 0 = corner ++ cellEdgeH ++ corner
makeEdgeSegmentH a = corner ++ cellEdgeH ++ makeEdgeSegmentH (a - 1)

makeGuideSegmentH :: Int -> [Char]
makeGuideSegmentH 0 = guideCorner ++ cellGuideH ++ guideCorner
makeGuideSegmentH a = guideCorner ++ cellGuideH ++ makeGuideSegmentH (a - 1)

boxEdgeH = makeEdgeSegmentH 2 
boxGuideH = makeGuideSegmentH 2
boxSpaceOnlyRow = verticalEdgeSegment ++ makeSpaceOnlyRowSegment(2)
boxSpaceRow = verticalEdgeSegment ++ makeSpaceRowSegment(2)

boxNumberRow :: [Char] -> [Char]
boxNumberRow a = verticalEdgeSegment ++ makeNumberRowSegment(a)

-- prints "     :     :     :"
makeSpaceOnlyRowSegment :: Int -> [Char]
makeSpaceOnlyRowSegment 0 = "     " ++ verticalGuideSegment 
makeSpaceOnlyRowSegment a = "     " ++ verticalGuideSegment ++ makeSpaceOnlyRowSegment(a - 1) 

makeSpaceRowSegment :: Int -> [Char]
makeSpaceRowSegment 0 = "  " ++ blue ++ "0  " ++ verticalGuideSegment 
makeSpaceRowSegment a = "  " ++ blue ++ (show a) ++ "  " ++ verticalGuideSegment ++ makeSpaceRowSegment(a - 1) 

makeNumberRowSegment :: [Char] -> [Char]
makeNumberRowSegment [] = ""
makeNumberRowSegment x = 
  let a:as = x
  in "  " ++ blue ++ [a] ++ "  " ++ verticalGuideSegment ++ makeNumberRowSegment(as) 

-- Merging
mergeEdges s =  replace (verticalGuideSegment ++ verticalEdgeSegment) (verticalEdgeSegment) (s)
mergeBoxEdges s = replace (gridSideEdge ++ green ++ gridSideEdge) (gridSideEdge) (s)
mergeCorners s = replace (doubleText corner) (corner) (s)
mergeGuideCorners s = replace (guideCorner ++ guideCorner) (corner) (s)

gridSideEdge = mergeCorners $ boxEdgeH ++ boxEdgeH ++ boxEdgeH  

gridHGuide = mergeGuideCorners $  boxGuideH ++ boxGuideH ++ boxGuideH 

gridSpaceRow = mergeEdges $ boxSpaceRow ++ boxSpaceRow ++ boxSpaceRow ++ verticalEdgeSegment 

gridNumberRow :: [Char] -> [Char]
gridNumberRow x =
  let a : b : c : d : e : f : g : h : i : [] = x
  in mergeEdges $ (boxNumberRow [a,b,c]) ++ (boxNumberRow [d,e,f]) ++ (boxNumberRow [g,h,i]) ++ verticalEdgeSegment


gridSpaceOnlyRow = mergeEdges $ boxSpaceOnlyRow ++ boxSpaceOnlyRow ++ boxSpaceOnlyRow ++ verticalEdgeSegment

-- Utils

doubleText s = s ++ s
appendNewLine a = a ++ "\n"
