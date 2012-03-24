{-
  Game of Life Core - provides the basic funtions used in the SDL version
  Copyright (C) 2012  Josh Chase

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module GameOfLife (Cell, glider, advanceGrid, alive) where

import Data.List
import System.Process as S

type Cell = (Int, Int)
xDim = 20
yDim = 20

glider :: Cell -> [Cell]
glider (x,y) = [(x+1,y),(x+2,y+1),(x,y+2),(x+1,y+2),(x+2,y+2)]

grid :: Int -> Int -> [[Cell]]
grid x y | y > 0 = map (\z -> (z,y)) [0..xDim] : (let y' = y - 1 in grid x y')
         | otherwise = [map (\z -> (z,y)) [0..xDim]]

-- List of points that may be livig next turn
interestingCells ps = concat . map neighbors $ ps

-- List of living points next turn
livingCells ps = nub . filter (\p -> testCell p ps) $ (interestingCells ps)

-- Probably a better way of doing this, but eh, whatever
neighbors :: Cell -> [Cell]
neighbors (x,y) = [(x+1,y+1),(x+1,y),(x,y+1),(x-1,y-1),(x-1,y),(x,y-1),(x-1,y+1),(x+1,y-1)]

-- Check if a point is 'alive'
alive :: Cell -> [Cell] -> Bool
alive p ps | intersect [p] ps == [] = False
           | otherwise = True

-- Populates the grid with a list of Cells.  Any Cells that are repeated or outside of the grid area are ignored.
populateGrid :: [Cell] -> [Cell]
populateGrid points = points

-- Advance the grid one turn
advanceGrid :: [Cell] -> [Cell]
advanceGrid ps = livingCells ps

-- Advance the grid n turns
advanceGridN :: Int -> [Cell] -> [Cell]
advanceGridN 0 ps = ps
advanceGridN n ps | n > 0 = advanceGridN (n-1) (advanceGrid ps)
                  | n < 0 = ps

-- Check the status of a point for the next iteration
testCell :: Cell -> [Cell] -> Bool
testCell p ps | alive p ps && (checkNeighbors p ps == 2 || checkNeighbors p ps == 3) = True
               | not (alive p ps) && (checkNeighbors p ps == 3) = True
               | otherwise = False

-- Check how many living neighbors a point has
checkNeighbors :: Cell -> [Cell] -> Int
checkNeighbors p ps = length . intersect ps $ (neighbors p)

-- Utility function to divide a list into even intervals
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery a lst = l1 : splitEvery a l2
  where (l1,l2) = splitAt a lst

-- Because I screwed up the rows/columns :P
rotateMatrix :: [[a]] -> [[a]]
rotateMatrix [] = []
rotateMatrix m = (map head m) : (rotateMatrix . filter (not . null) $ (map tail m))

-- Generates a string for a list of points based on whether they're alive
genString :: [Cell] -> [Cell] -> String
genString live pts = foldr (\x acc -> if alive x live then '#' : acc else ' ':acc) "" pts

-- Display the grid
showGrid :: [Cell] -> IO [()]
showGrid ps = sequence . map putStrLn $ stringList
  where stringList = map (genString ps) (grid xDim yDim)

-- Reads a Int from the user's input String.  If none is found, it defaults to 1
parseInput :: String -> Int
parseInput s = case reads s of
                    [(x,_)] -> x
                    _ -> 1
