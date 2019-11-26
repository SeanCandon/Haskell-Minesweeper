module Minesweeper where

import System.Random
import Data.Sort
import Data.List

-- Box data type:
-- Bool => marked
-- Bool => cleared
-- Int => row of grid
-- Int => column of grid
-- Int => 0 for mine, 1 for non-mine
-- [(Int,Int)] => list if i and j values of box's neighbours in the grid
data Box = Box Bool Bool Int Int Int [(Int,Int)] deriving Eq

-- function to create a box. passed in are grid size, position in grid and whether
-- or not box is a mine. Box has not been marked or cleared by default. Neighbours
-- determined by whether or not box is in corner or on edge of the grid
createbox :: Int -> Int -> Int -> Int -> Box
createbox max r c v =
                  if r == 0 && c == 0 then
                    Box False False r c v [(r+1,c), (r,c+1), (r+1,c+1)]
                  else if r == max && c == max then
                    Box False False r c v [(r-1,c), (r,c-1), (r-1,c-1)]
                  else if r == 0 && c == max then
                    Box False False r c v [(r+1,c), (r,c-1), (r+1,c-1)]
                  else if r == max && c == 0 then
                    Box False False r c v [(r-1,c), (r,c+1), (r-1,c+1)]
                  else if r == 0 then
                    Box False False r c v [(r,c-1),(r,c+1),(r+1,c-1),(r+1,c),(r+1,c+1)]
                  else if r == max then
                    Box False False r c v [(r,c-1),(r,c+1),(r-1,c-1),(r-1,c),(r-1,c+1)]
                  else if c == 0 then
                    Box False False r c v [(r+1,c),(r+1,c+1),(r,c+1),(r-1,c),(r-1,c+1)]
                  else if c == max then
                    Box False False r c v [(r+1,c),(r+1,c-1),(r,c-1),(r-1,c),(r-1,c-1)]
                  else
                    Box False False r c v [(r,c-1),(r,c+1),(r-1,c-1),(r-1,c),(r-1,c+1),(r+1,c-1),(r+1,c),(r+1,c+1)]

-- creates a list of boxes by creating each box one at a time with the
-- correct i and j values according to the desired size of the grid. row param
-- starts at 0. function creates grid one row at a time.
createGrid :: Int -> Int -> [Box] -> [Box]
createGrid row size list
  | (row == size) = list -- grid finished
  -- create and append row to grid
  | otherwise = createGrid (row+1) size (list ++ (createRow row 0 size []))

-- function to create row to be added to a grid. takes in row number (the i val
-- for each box that will be created for the row) and increments column number
-- from 0 to size. for each column number a new box is created and added to the
-- row.
createRow :: Int -> Int -> Int -> [Box] -> [Box]
createRow row col size list
  | (col == size) = list -- row finished
  -- create box and append to row
  | otherwise = createRow row (col+1) size (list ++ [createbox (size-1) row col 1])

-- function to get box from grid using i and j values. if not found then a box
-- with impossible values is returned.
getBox :: Int -> Int -> [Box] -> Box
getBox _ _ [] = (Box False False (-1) (-1) 0 []) -- impossible box
getBox r c ((Box m ch x y v n):bs)
  | (r == x) && (c == y) = (Box m ch x y v n) -- box found
  | otherwise            = getBox r c bs -- keep looking

-- function to traverse grid and see if a non-mine has yet to be cleared. if so,
-- the user has not won. if no such box is found, the win condition is
-- satisfied and the user has won
win :: [Box] -> Bool
win [] = True -- won
win ((Box _ ch _ _ v _):boxes) |(ch==False) && (v==1) = False -- lost
                             |otherwise = win boxes

-- function to check if a box is a mine or not. if it is, then the user is
-- dead. if not, the user is still alive.
alive :: Box -> Bool
alive (Box _ _ _ _ 1 _) = True
alive (Box _ _ _ _ 0 _) = False

-- function to select a box in grid. clears box, will clear neighbours
-- of box if none of them are mines. will then attempt to do the same for each of
-- the neighbours if all neighbours are non-mines. Returns updated grid
select :: Box -> [Box] -> [Box]
-- if mine then just clear the box
select (Box m False r c 0 n) grid = clear (Box m False r c 0 n) grid []
-- if mine and has been cleared then do nothing and return grid unchanged
select (Box m True r c 0 n) grid = grid
-- if non-mine and uncleared then
select (Box m ch r c v n) grid = do
  let mines = countMines grid n 0
  -- if there are no neighbouring mines, clear neighbours and then attempt to
  -- clear the neighbours of the neighbours, if none of those are mines
  if mines == 0 then do
    let ngs = unclearedNeighbours grid n [] -- gets uncleared neighbours
    let g = clear (Box m ch r c v n) grid [] -- clears selected box
    let gr = clearNeighbours g ngs [] -- clears immediate neighbours
    clearBeyond gr ngs -- attempt to clear neighbours of neighbours
  else -- if there are neighbouring mines just clear selected box
    clear (Box m ch r c v n) grid []

-- function to clear list of boxes passed in. Takes in whole grid and iterates
-- over it. returns updated grid with boxes in list cleared
clearNeighbours :: [Box] -> [Box] -> [Box] -> [Box]
clearNeighbours [] _ newgrid = newgrid
clearNeighbours (b:bs) ns newgrid = do
  -- if current box in in grid matches one of the boxes to clear,
  -- clear the box and append it to new grid
  if (elem b ns) == True then do
    let (Box m ch r c v n) = b
    clearNeighbours bs ns (newgrid ++ [(Box m True r c v n)])
  else -- otherwise append box unchanged
    clearNeighbours bs ns (newgrid ++ [b])

-- function that iterates over list of boxes and calls select on each one.
-- as defined above select will clear box selected and if there are no neighbouring
-- mines it will clear its neighbours and then call this function to attempt to
-- clear its neighbours' neighbours.
clearBeyond :: [Box] -> [Box] -> [Box]
clearBeyond grid [] = grid
clearBeyond grid (n:ns) = clearBeyond (select n grid) ns

-- takes in a grid and a list of i,j pairs and then returns a list of those
-- boxes in the grid corresponding to those i and j values that are uncleared.
unclearedNeighbours :: [Box] -> [(Int,Int)] -> [Box] -> [Box]
unclearedNeighbours _ [] nbs = nbs
unclearedNeighbours grid ((r,c):ns) nbs = do
  let (Box m ch x y v n)= getBox r c grid -- get box in grid with those i j vals
  -- in uncleared then add to list
  if ch == False then
    unclearedNeighbours grid ns (nbs ++ [(Box m ch x y v n)])
  else -- otherwise skip
    unclearedNeighbours grid ns nbs

-- takes in a grid and a box and returns that grid updated with that box now
-- cleared. all else in the grid remains the same.
clear :: Box -> [Box] -> [Box] -> [Box]
clear _ [] newgrid = newgrid
clear (Box m ch r c v n) (b:bs) newgrid
  | ((Box m ch r c v n) == b) = clear (Box m ch r c v n) bs (newgrid ++ [(Box m True r c v n)])
  | otherwise = clear (Box m ch r c v n) bs (newgrid ++ [b])

-- function called to get how many neighbouring mines a box has
noOfMines :: Box -> [Box] -> Int
noOfMines (Box _ _ _ _ _ n) grid = countMines grid n 0

-- function to count the number of mines given a grid and a list of i,j pairs
-- corresponding to boxes in that grid. returns the number of those
-- corresponding boxes are mines.
countMines :: [Box] -> [(Int,Int)] -> Int -> Int
countMines _ [] mines = mines
countMines grid ((r,c):indices) mines = do
  let (Box _ _ _ _ v _) = getBox r c grid
  if v == 0 then -- if mine then add to count and go to next i,j pair
    countMines grid indices (mines+1)
  else -- if not mine then don't add to count, just continue
    countMines grid indices mines

-- given a list of boxes returns the number of those boxes that are marked
noOfMarked :: [Box] -> Int -> Int
noOfMarked [] count = count
noOfMarked ((Box m _ _ _ _ _):boxes) count
  | (m == True) = noOfMarked boxes (count+1) -- if marked add to count
  | otherwise = noOfMarked boxes count -- if not marked don't add to count

-- takes in a box and a grid. if box is unmarked then it is marked and an updated
-- grid is returned. if already marked then it is unmarked and an updated grid
-- is returned
markAsMine :: Box -> [Box] -> [Box] -> [Box]
markAsMine _ [] newgrid = newgrid
markAsMine (Box m ch r c v n) (b:bs) newgrid
  | (((Box m ch r c v n) == b) && m==False) = markAsMine (Box m ch r c v n) bs (newgrid ++ [(Box True ch r c v n)])
  | (((Box m ch r c v n) == b) && m==True) = markAsMine (Box m ch r c v n) bs (newgrid ++ [(Box False ch r c v n)])
  | otherwise = markAsMine (Box m ch r c v n) bs (newgrid ++ [b])

-- function to pick the safest possible move based on probability. if a
-- probability of being a mine or not cannot be assigned to any boxes then
-- Nothing is returned. otherwise the box in the grid with the lowest score is
-- returned
pickSafestMove :: [Box] -> Maybe Box
pickSafestMove grid = do
  -- create list of uncleared and unmarked boxes along with their respective
  -- scores. explained in greater detail below
  let p = listOfProbabilities grid grid []
  if p == [] then Nothing
  else
    Just (safestMove grid p (head $ p)) -- returns box with lowest score

-- recurses over a list of (score,box) values and returns the box with the
-- lowest score.
safestMove :: [Box] -> [(Float,Box)] -> (Float,Box) -> Box
safestMove _ [] (_,safest) = safest
safestMove grid ((score,b):rest) (curr,b2)
  | (score < curr) = safestMove grid rest (score,b)
  | otherwise = safestMove grid rest (curr,b2)

-- given a grid returns a list of boxes in the grid that are uncleared and
-- unmarked alongside their respective scores. a box's score is the sum of
-- probabilities that it is a mine. a box is only added to this list if the
-- probability of it being a mine can be reasoned from what the user can see of
-- the grid.
listOfProbabilities :: [Box] -> [Box] -> [(Float,Box)] -> [(Float,Box)]
listOfProbabilities _ [] probs = probs
-- iterate over grid, ignore box if uncleared or a mine
listOfProbabilities grid ((Box m False r c v n):boxes) probs = listOfProbabilities grid boxes probs
listOfProbabilities grid ((Box m ch r c 0 n):boxes) probs = listOfProbabilities grid boxes probs
-- if cleared non-mine found
listOfProbabilities grid ((Box m ch r c v n):boxes) probs = do
  let mines = noOfMines (Box m ch r c v n) grid -- get no. of neighbouring mines
  let gn = unclearedNeighbours grid n [] -- get all uncleared neighbours
  let marked = noOfMarked gn 0 -- get no. of marked uncleared neighbours
  let remainingMines = mines - marked
  -- if user has marked the correct number of mines or has marked every
  -- uncleared neighbour, the list of probabilities is not edited
  if (marked == (length gn)) || (remainingMines <= 0) then
    listOfProbabilities grid boxes probs
  -- otherwise calculate the probability that each of the uncleared unmarked
  -- neighbours are mines (should be same probability for each) and insert
  -- these probabilities into the list of probabilities
  else do
    let remainingNeighbours = (length gn) - marked
    let prob = (fromIntegral remainingMines) / (fromIntegral remainingNeighbours)
    let new = insertProbabilities probs gn prob
    listOfProbabilities grid boxes new

-- takes in a single probability value and a list of boxes and applies that
-- probability to each of the boxes in the list of probabilities.
insertProbabilities :: [(Float,Box)] -> [Box] -> Float -> [(Float,Box)]
insertProbabilities probs [] _ = probs
insertProbabilities probs ((Box m ch r c v n):ns) prob
  | (m == True) = insertProbabilities probs ns prob -- ignore box if marked
  | otherwise = insertProbabilities (insertProb probs (Box m ch r c v n) prob []) ns prob

-- inserts single probability/box pair into the list of probabilities. If the box
-- is already in the list then the probability is simply added to the box's
-- score. if not then the box is added with its score initially set to be the
-- probability value.
insertProb :: [(Float,Box)] -> Box -> Float -> [(Float,Box)] -> [(Float,Box)]
-- if new, add new pair
insertProb [] box prob newprobs = (newprobs ++ [(prob,box)])
insertProb ((p,b):ps) box prob newprobs
  -- if box already present then add prob val to existing score
  | (b == box) = insertProb ps box prob (newprobs ++ [(p+prob,box)])
  | otherwise = insertProb ps box prob (newprobs ++ [(p,b)])

-- function to check if there are any moves available for the AI player. if
-- all boxes have been either cleared or marked then the AI player can do
-- nothing.
movesAvailable :: [Box] -> Bool
movesAvailable [] = False
movesAvailable ((Box m ch _ _ _ _):boxes)
  | (m == False) || (ch == False) = True
  | otherwise = movesAvailable boxes

-- takes in a grid, the number of mines to be added, the potential positions of
-- the mines in the grid, and the box to avoid. mines will be added to the grid
-- anywhere except for said box and its immediate neighbours.
addMines :: [Box] -> Int -> [Int] -> Box -> [Box]
addMines grid mineAmount indices button = do
  -- get all boxes in grid corresponding to the potential mine positions
  -- except for the selected box and its neighbours
  let nobad = removeBadMines grid indices button []
  -- take the required number of boxes from that list. these are going to be
  -- the mines
  let mines = take mineAmount nobad
  layMines grid mines [] -- convert these boxes to mines

-- takes in a grid and list of boxes in the grid and returns that grid with
-- all those boxes in the list converted to mines.
layMines :: [Box] -> [Box] -> [Box] -> [Box]
layMines [] _ newgrid = newgrid
layMines (x:xs) mines newgrid = do
  -- if mine found convert to mine and add to new grid
  if (elem x mines) then do
    let (Box m ch r c v n) = x
    layMines xs mines (newgrid ++ [(Box m ch r c 0 n)])
  else -- otherwise append unchanged box to new grid
    layMines xs mines (newgrid ++ [x])

-- gets all boxes in grid corresponding to the list of indices passed in
-- except for the selected box and its neighbours
removeBadMines :: [Box] -> [Int] -> Box -> [Box] -> [Box]
removeBadMines _ [] _ goodmines = goodmines
removeBadMines grid (i:is) button goodmines = do
  let p = grid !! i -- get ith box in grid
  let (Box _ _ r c _ _) = p
  let (Box _ _ _ _ _ n) = button
  --if grid[i] is equal to the selected box or is among its neighbours then ignore
  if (p == button) || (elem (r,c) n) then
    removeBadMines grid is button goodmines
  else -- otherwise append to list
    removeBadMines grid is button (goodmines ++ [p])
