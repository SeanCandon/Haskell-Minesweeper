module Minesweeper where

import System.Random
import Data.Sort
import Data.List

data Box = Box Bool Bool Int Int Int [(Int,Int)] deriving Eq

play :: IO ()
play = do
          g <- newStdGen
          let size = 20
          let mineAmount = size * 2
          let mineIndexes = (take (mineAmount+9) . nub $ (randomRs (0,(size*size)-1) g :: [Int]))
          print mineIndexes
          let b = createbox (size-1) 8 2 0
          let gr = createGrid 0 size []
          let grid = addMines gr mineAmount mineIndexes b
          dispGrid (revealAll grid [])
          putStrLn "---------------------------------------------------------------------"
          let ng = select (getBox 8 2 grid) grid
          dispGrid ng
          takeInput ng

takeInput :: [Box] -> IO ()
takeInput [] = print "Done"
takeInput grid = do
                  putStrLn "Enter row: "
                  row <- getLine
                  putStrLn "Enter column: "
                  col <- getLine
                  let r = read row :: Int
                  let c = read col :: Int
                  let box = getBox r c grid
                  let (Box _ _ _ _ v _) = box
                  if ((exist box == 1)) then
                    if v == 0 then do
                      let newgrid = select box grid
                      dispGrid newgrid
                      putStrLn "dead"
                    else do
                      let newgrid = select box grid
                      dispGrid newgrid
                      printProbs $ listOfProbabilities newgrid newgrid []
                      let (Box _ _ x y _ n) = unwrapBox (pickSafestMove newgrid)
                      if (n == []) then takeInput newgrid
                      else do
                        let ss = "safest = (" ++ (show x) ++ "," ++ (show y) ++ ")"
                        putStrLn ss
                        takeInput newgrid
                  else
                    do
                      putStrLn "Box does not exist"
                      takeInput grid


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

dispBox :: Box -> IO ()
dispBox (Box _ True _ _ v _) = do
                        putStr "|"
                        putStr $ show v
                        putStr "|"
dispBox (Box _ False _ _ _ _) = putStr "|?|"

select :: Box -> [Box] -> [Box]
select (Box m False r c 0 n) grid = reveal (Box m False r c 0 n) grid []
select (Box m True r c 0 n) grid = grid
select (Box m ch r c v n) grid = do
  let mines = countMines grid n 0
  if mines == 0 then do
    let ngs = uncheckedNeighbours grid n []
    let g = reveal (Box m ch r c v n) grid []
    let gr = revealNeighbours g ngs []
    revealBeyond gr ngs
  else
    reveal (Box m ch r c v n) grid []

revealNeighbours :: [Box] -> [Box] -> [Box] -> [Box]
revealNeighbours [] _ newgrid = newgrid
revealNeighbours (b:bs) ns newgrid = do
  if (elem b ns) == True then do
    let (Box m ch r c v n) = b
    revealNeighbours bs ns (newgrid ++ [(Box m True r c v n)])
  else
    revealNeighbours bs ns (newgrid ++ [b])

revealBeyond :: [Box] -> [Box] -> [Box]
revealBeyond grid [] = grid
revealBeyond grid (n:ns) = revealBeyond (select n grid) ns

uncheckedNeighbours :: [Box] -> [(Int,Int)] -> [Box] -> [Box]
uncheckedNeighbours _ [] nbs = nbs
uncheckedNeighbours grid ((r,c):ns) nbs = do
  let (Box m ch x y v n)= getBox r c grid
  if ch == False then
    uncheckedNeighbours grid ns (nbs ++ [(Box m ch x y v n)])
  else
    uncheckedNeighbours grid ns nbs

reveal :: Box -> [Box] -> [Box] -> [Box]
reveal _ [] newgrid = newgrid
reveal (Box m ch r c v n) (b:bs) newgrid
  | ((Box m ch r c v n) == b) = reveal (Box m ch r c v n) bs (newgrid ++ [(Box m True r c v n)])
  | otherwise = reveal (Box m ch r c v n) bs (newgrid ++ [b])


noOfMines :: Box -> [Box] -> Int
noOfMines (Box _ _ _ _ v n) grid |(v == 1) = countMines grid n 0
                                 |otherwise = (countMines grid n 0) - 1

noOfMarked :: [Box] -> Int -> Int
noOfMarked [] count = count
noOfMarked ((Box m _ _ _ _ _):boxes) count
  | (m == True) = noOfMarked boxes (count+1)
  | otherwise = noOfMarked boxes count

countMines :: [Box] -> [(Int,Int)] -> Int -> Int
countMines _ [] mines = mines
countMines grid ((r,c):indices) mines = do
  let (Box _ _ _ _ v _) = getBox r c grid
  if v == 0 then
    countMines grid indices (mines+1)
  else
    countMines grid indices mines

markAsMine :: Box -> [Box] -> [Box] -> [Box]
markAsMine _ [] newgrid = newgrid
markAsMine (Box m ch r c v n) (b:bs) newgrid
  | (((Box m ch r c v n) == b) && m==False) = markAsMine (Box m ch r c v n) bs (newgrid ++ [(Box True ch r c v n)])
  | (((Box m ch r c v n) == b) && m==True) = markAsMine (Box m ch r c v n) bs (newgrid ++ [(Box False ch r c v n)])
  | otherwise = markAsMine (Box m ch r c v n) bs (newgrid ++ [b])

unwrapBox :: Maybe Box -> Box
unwrapBox Nothing = (Box False False (-1) (-1) 0 [])
unwrapBox (Just b) = b

pickSafestMove :: [Box] -> Maybe Box
pickSafestMove grid = do
  let p = listOfProbabilities grid grid []
  if p == [] then Nothing
  else
    Just (safestMove grid p (head $ p))

safestMove :: [Box] -> [(Float,Box)] -> (Float,Box) -> Box
safestMove _ [] (_,safest) = safest
safestMove grid ((score,b):rest) (curr,b2)
  | (score < curr) = safestMove grid rest (score,b)
  | otherwise = safestMove grid rest (curr,b2)

listOfProbabilities :: [Box] -> [Box] -> [(Float,Box)] -> [(Float,Box)]
listOfProbabilities _ [] probs = probs
listOfProbabilities grid ((Box m False r c v n):boxes) probs = listOfProbabilities grid boxes probs
listOfProbabilities grid ((Box m ch r c 0 n):boxes) probs = listOfProbabilities grid boxes probs
listOfProbabilities grid ((Box m ch r c v n):boxes) probs = do
  let mines = noOfMines (Box m ch r c v n) grid
  let gn = uncheckedNeighbours grid n []
  let marked = noOfMarked gn 0
  let remainingMines = mines - marked
  if (marked == (length gn)) || (remainingMines <= 0) then
    listOfProbabilities grid boxes probs
  else do
    let remainingNeighbours = (length gn) - marked
    let prob = (fromIntegral remainingMines) / (fromIntegral remainingNeighbours)
    let new = insertProbabilities probs gn prob
    listOfProbabilities grid boxes new

insertProbabilities :: [(Float,Box)] -> [Box] -> Float -> [(Float,Box)]
insertProbabilities probs [] _ = probs
insertProbabilities probs ((Box m ch r c v n):ns) prob
  | (m == True) = insertProbabilities probs ns prob
  | otherwise = insertProbabilities (insertProb probs (Box m ch r c v n) prob []) ns prob

insertProb :: [(Float,Box)] -> Box -> Float -> [(Float,Box)] -> [(Float,Box)]
insertProb [] box prob newprobs = (newprobs ++ [(prob,box)])
insertProb ((p,b):ps) box prob newprobs
  | (b == box) = insertProb ps box prob (newprobs ++ [(p+prob,box)])
  | otherwise = insertProb ps box prob (newprobs ++ [(p,b)])

printProbs :: [(Float,Box)] -> IO ()
printProbs [] = return ()
printProbs ((p,(Box _ _ r c _ _)):ps) = do
  let s = "|" ++ (show r) ++ " " ++ (show c) ++ " " ++ (show p) ++ "|"
  putStr s
  printProbs ps

getBox :: Int -> Int -> [Box] -> Box
getBox _ _ [] = (Box False False (-1) (-1) 0 [])
getBox r c ((Box m ch x y v n):bs)
                                | (r == x) && (c == y) = (Box m ch x y v n)
                                | otherwise            = getBox r c bs

exist :: Box -> Int
exist (Box _ _ _ _ _ []) = 0
exist b = 1

win :: [Box] -> Int
win [] = 1
win ((Box _ ch _ _ v _):boxes) |(ch==False) && (v==1) = 0
                             |otherwise = win boxes

alive :: Box -> Bool
alive (Box _ _ _ _ 1 _) = True
alive (Box _ _ _ _ 0 _) = False

movesAvailable :: [Box] -> Bool
movesAvailable [] = False
movesAvailable ((Box m ch _ _ _ _):boxes)
  | (m == False) || (ch == False) = True
  | otherwise = movesAvailable boxes

revealAll :: [Box] -> [Box] -> [Box]
revealAll [] new = new
revealAll ((Box m _ r c v n):bs) new = revealAll bs (new ++ [(Box m True r c v n)])

createGrid :: Int -> Int -> [Box] -> [Box]
createGrid row size list
  | (row == size) = list
  | otherwise = createGrid (row+1) size (list ++ (createRow row 0 size []))

createRow :: Int -> Int -> Int -> [Box] -> [Box]
createRow row col size list
  | (col == size) = list
  | otherwise = createRow row (col+1) size (list ++ [createbox (size-1) row col 1])

addMines :: [Box] -> Int -> [Int] -> Box -> [Box]
addMines grid mineAmount indices button = do
  let nobad = removeBadMines grid indices button []
  let mines = take mineAmount nobad
  layMines grid mines []

layMines :: [Box] -> [Box] -> [Box] -> [Box]
layMines [] _ newgrid = newgrid
layMines (x:xs) mines newgrid = do
  if (elem x mines) then do
    let (Box m ch r c v n) = x
    layMines xs mines (newgrid ++ [(Box m ch r c 0 n)])
  else
    layMines xs mines (newgrid ++ [x])

removeBadMines :: [Box] -> [Int] -> Box -> [Box] -> [Box]
removeBadMines _ [] _ goodmines = goodmines
removeBadMines grid (i:is) button goodmines = do
  let p = grid !! i
  let (Box _ _ r c _ _) = p
  let (Box _ _ _ _ _ n) = button
  if (p == button) || (elem (r,c) n) then
    removeBadMines grid is button goodmines
  else
    removeBadMines grid is button (goodmines ++ [p])

dispGrid :: [Box] -> IO ()
dispGrid boxes = dispGridCount 0 (sqrt (fromIntegral (length boxes))) boxes

dispGridCount :: (Floating a, Eq a) => a-> a -> [Box] -> IO ()
dispGridCount _ _ [b] = do
                          dispBox b
                          putStr "\n"
dispGridCount count len (b:bs) =
                          if count == len-1 then
                          do
                            dispBox b
                            putStr "\n"
                            dispGridCount 0 len bs
                          else
                          do
                            dispBox b
                            dispGridCount (count+1) len bs
