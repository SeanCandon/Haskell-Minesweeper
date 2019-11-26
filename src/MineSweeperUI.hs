module MineSweeperUI
where

import qualified Graphics.UI.Threepenny as UI
import qualified Graphics.UI.Threepenny.Core as Core
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.JQuery
import Graphics.Gloss.Interface.Environment

import System.Random
import Data.Sort
import Data.List
import Control.Monad

import Minesweeper

start :: IO ()
start = do
  wh <- getScreenSize
  let size = 14
  let buttonSize = getOptimalPix wh size
  let grid = createGrid 0 size []
  startGUI defaultConfig (draw True True buttonSize grid grid size [])

draw :: Bool -> Bool -> (Int,Int) -> [Box] -> [Box] -> Int -> [UI Element] -> Window -> UI ()
draw first alive bs [] gr size uielems window = do
  let final = convertGrid uielems 0 size []
  g <- grid final
  if alive == True then do
    if first == True then do
      getBody window #+ [return g]
      return ()
    else do
      b <- UI.button #+ [string "Play Safe"]
                     # set style [("width","100px"),("height","100px")]
                     # set (attr "class") ("buttonboi")
      getBody window #+ [return b]
      getBody window #+ [return g]
      return ()
      on UI.click b $ \_ -> do
        let box = pickSafestMove gr
        playSafe window box gr size bs
  else do
    getBody window #+ [ UI.div #+ [string "You are dead"] ]
    getBody window #+ [return g]
    return ()

draw first alive bs ((Box m ch r c v n):boxes) grid size uielems window = do
  let (width,height) = bs
  let w = (show width) ++ "px"
  let h = (show height) ++ "px"
  if ch == False then
    do
      if (m == False) then do
        b <- UI.button # set style [("width",w),("height",h)]
                       # set (attr "class") ("buttonboi")
                       # set (attr "oncontextmenu") ("return false;")
        draw first alive bs boxes grid size (uielems ++ [return b]) window

        on UI.click b $ \_ -> do
          clearSquare window (Box m ch r c v n) grid size bs first

        on UI.contextmenu b $ \_ -> do
          markSquare window (Box m ch r c v n) grid size bs first

      else do
        if (alive == False) && (v==0) then do
          b <- UI.button # set style [("background-color","red"),("width",w),("height",h)]
                         # set (attr "class") ("buttonboi")
          draw first alive bs boxes grid size (uielems ++ [return b]) window
        else do
          b <- UI.button # set text "?"
                         # set style [("background-color","blue"),("width",w),("height",h)]
                         # set (attr "class") ("buttonboi")
                         # set (attr "oncontextmenu") ("return false;")
          draw first alive bs boxes grid size (uielems ++ [return b]) window
          on UI.click b $ \_ -> do
              clearSquare window (Box m ch r c v n) grid size bs False
          on UI.contextmenu b $ \_ -> do
              markSquare window (Box m  ch r c v n) grid size bs False

  else do
    if v == 1 then do
        let no = noOfMines (Box m ch r c v n) grid
        b <- UI.button # set text (show no)
                       # set style [("background-color","green"),("width",w),("height",h)]
                       # set (attr "class") ("buttonboi")
        draw first alive bs boxes grid size (uielems ++ [return b]) window
    else do
        b <- UI.button # set style [("background-color","red"),("width",w),("height",h)]
                       # set (attr "class") ("buttonboi")
        draw first alive bs boxes grid size (uielems ++ [return b]) window

convertGrid :: [UI Element] -> Int -> Int -> [[UI Element]] -> [[UI Element]]
convertGrid [] _ _ new = new
convertGrid buttons count size new = do
  if count == size-1 then do
    let row = take size buttons
    let rest = drop size buttons
    convertGrid rest 0 size (new ++ [row])
  else
    convertGrid buttons (count+1) size new

eraseWindow :: Window -> [Element] -> UI ()
eraseWindow _ [] = return ()
eraseWindow window (el:elems) = do
  Core.delete el
  eraseWindow window elems

deleteEl :: (Maybe Element) -> UI ()
deleteEl Nothing = return ()
deleteEl (Just el) = Core.delete el

clearSquare :: Window -> Box -> [Box] -> Int -> (Int,Int) -> Bool -> UI ()
clearSquare window box grid size bs f = do
  if f == True then do
    afterFirstClick window box grid size bs f
  else do
    let newgrid = select box grid
    let w = win newgrid
    if (w == 1) then do
      getBody window #+ [ UI.div #+ [ string "You win!"] ]
      return ()
    else do
      h <- getElementsByClassName window "buttonboi"
      eraseWindow window h
      draw False (alive box) bs newgrid newgrid size [] window

markSquare :: Window -> Box -> [Box] -> Int -> (Int,Int) -> Bool -> UI ()
markSquare window box grid size bs f = do
  if f == False then do
    h <- getElementsByClassName window "buttonboi"
    eraseWindow window h
    let newgrid = markAsMine box grid []
    draw f True bs newgrid newgrid size [] window
  else
    return ()

afterFirstClick :: Window -> Box -> [Box] -> Int -> (Int,Int) -> Bool -> UI ()
afterFirstClick window box grid size bs f = do
  g <- liftIO newStdGen
  let mineAmount = size * 2
  let mineIndexes = sort (take (mineAmount+9) . nub $ (randomRs (0,(size*size)-1) g :: [Int]))
  let ng = addMines grid mineAmount mineIndexes box
  let newgrid = select box ng
  h <- getElementsByClassName window "buttonboi"
  eraseWindow window h
  draw False True bs newgrid newgrid size [] window

playSafe :: Window -> Maybe Box -> [Box] -> Int -> (Int,Int) -> UI ()
playSafe window Nothing grid size bs = do
  if (movesAvailable grid) == True then
    clearRandom window grid size bs
  else
    return ()
playSafe window (Just b) grid size bs =
  clearSquare window b grid size bs False

clearRandom :: Window -> [Box] -> Int -> (Int,Int) -> UI ()
clearRandom window grid size bs = do
  g1 <- liftIO newStdGen
  let (i,g2) = randomR (0,size-1) g1
  let (j,_) = randomR (0,size-1) g2
  let (Box m ch r c v n) = getBox i j grid
  if (m == True) || (ch == True) then
    clearRandom window grid size bs
  else clearSquare window (Box m ch r c v n) grid size bs False

getOptimalPix :: (Int,Int) -> Int -> (Int,Int)
getOptimalPix (width,height) size = do
  let w = floor ((fromIntegral width) * (0.5))
  let h = floor ((fromIntegral height) * (0.5))
  ((w `div` size),(h `div` size))
