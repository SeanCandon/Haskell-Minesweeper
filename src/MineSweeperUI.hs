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

-- starts the program. defines N of NxN grid, finds the optimal
-- button size, creates grid (1d list of Boxes) and calls draw function.
start :: IO ()
start = do
  wh <- getScreenSize
  let size = 10
  let buttonSize = getOptimalPix wh size
  let grid = createGrid 0 size []
  startGUI defaultConfig (draw True True buttonSize grid grid size [])

-- draw recurses through grid of Boxes and translates those to UI Elements,
-- appending them to a list as it goes. Base case is when there's no more boxes
-- left in grid, and the UI Elements are converted into a 2d list and then into
-- a grid, which is then drawn in the window.
draw :: Bool -> Bool -> (Int,Int) -> [Box] -> [Box] -> Int -> [UI Element] -> Window -> UI ()
-- base case of draw function
draw first alive bs [] gr size uielems window = do
  let final = convertGrid uielems 0 size [] -- converts buttons to 2d list
  g <- grid final -- converts buttons to grid
  if alive == True then do
    -- if user is on first click then only display the grid
    if first == True then do
      getBody window #+ [return g]
      return ()
    -- if user has already clicked then also display button to select cell in
    -- the grid with the lowest probability of being a mine.
    else do
      -- create play safe button
      b <- UI.button #+ [string "Play Safe"]
                     # set style [("width","100px"),("height","100px")]
                     # set (attr "class") ("buttonboi")
      getBody window #+ [return b]
      getBody window #+ [return g]
      return ()
      -- if play safe button is pressed
      on UI.click b $ \_ -> do
        let box = pickSafestMove gr
        playSafe window box gr size bs
  -- if user is dead (selected a mine) then print message and return
  else do
    getBody window #+ [ UI.div #+ [string "You are dead"] ]
    getBody window #+ [return g]
    return ()
-- recursive case of draw function. recurses over boxes and converts them to
-- UI Elements/buttons.
draw first alive bs ((Box m ch r c v n):boxes) grid size uielems window = do
  -- optimal button size stuff
  let (width,height) = bs
  let w = (show width) ++ "px"
  let h = (show height) ++ "px"
  -- if current box in grid has not been checked/cleared yet by user
  if ch == False then
    do
      if (m == False) then do -- if current box is not marked as a mine
        -- button is created and added to list of buttons, draw is called on
        -- next box in list of boxes
        b <- UI.button # set style [("width",w),("height",h)]
                       # set (attr "class") ("buttonboi")
                       # set (attr "oncontextmenu") ("return false;")
        draw first alive bs boxes grid size (uielems ++ [return b]) window
        -- if button is left clicked, it is cleared
        on UI.click b $ \_ -> do
          clearSquare window (Box m ch r c v n) grid size bs first
        -- if button is right clicked, it is marked
        on UI.contextmenu b $ \_ -> do
          markSquare window (Box m ch r c v n) grid size bs first

      else do -- if current box is marked
        -- if user is dead and box is a mine then a red button is created
        -- button is not clickable if user is dead
        if (alive == False) && (v==0) then do
          b <- UI.button # set style [("background-color","red"),("width",w),("height",h)]
                         # set (attr "class") ("buttonboi")
          draw first alive bs boxes grid size (uielems ++ [return b]) window
        else do -- otherwise a blue button is created for when box is marked
          b <- UI.button # set style [("background-color","blue"),("width",w),("height",h)]
                         # set (attr "class") ("buttonboi")
                         # set (attr "oncontextmenu") ("return false;")
          draw first alive bs boxes grid size (uielems ++ [return b]) window
          -- on left click the box is cleared, on right click it is unmarked
          on UI.click b $ \_ -> do
              clearSquare window (Box m ch r c v n) grid size bs False
          on UI.contextmenu b $ \_ -> do
              markSquare window (Box m  ch r c v n) grid size bs False

  else do -- if current box has been cleared
    -- if box is not a mine then a green button is created and it shows how many
    -- mines surround the box
    if v == 1 then do
        let no = noOfMines (Box m ch r c v n) grid -- find no of neighbouring mines
        b <- UI.button # set text (show no)
                       # set style [("background-color","green"),("width",w),("height",h)]
                       # set (attr "class") ("buttonboi")
        draw first alive bs boxes grid size (uielems ++ [return b]) window
    -- if box is a mine then a red button is made
    else do
        b <- UI.button # set style [("background-color","red"),("width",w),("height",h)]
                       # set (attr "class") ("buttonboi")
        draw first alive bs boxes grid size (uielems ++ [return b]) window

-- function that converts a 1d list of UI Elements into a 2d list. This is
-- necessary to transform UI Elements into a grid element to draw
convertGrid :: [UI Element] -> Int -> Int -> [[UI Element]] -> [[UI Element]]
convertGrid [] _ _ new = new
convertGrid buttons count size new = do
  if count == size-1 then do
    let row = take size buttons
    let rest = drop size buttons
    convertGrid rest 0 size (new ++ [row])
  else
    convertGrid buttons (count+1) size new

-- function to erase list of elements from the window
eraseWindow :: Window -> [Element] -> UI ()
eraseWindow _ [] = return ()
eraseWindow window (el:elems) = do
  Core.delete el
  eraseWindow window elems

-- function to clear a box in the grid. If this is attempted on user's first
-- click then afterFirstClick is called, in which the mines are added to the
-- grid in a way to avoid the selected box and its neighbours. Before this the
-- grid has no mines.
-- if not on first click then the box is cleared. the function then checks if
-- the win condition is satisfied, where all non-mines have been cleared. if not
-- then all elements in the window are erased and the new grid (with the selected
-- box now cleared) is drawn in its place. Also passed in to draw is whether or
-- not the user is now dead. this is done wih the boolean function "alive"
clearSquare :: Window -> Box -> [Box] -> Int -> (Int,Int) -> Bool -> UI ()
clearSquare window box grid size bs f = do
  -- if first click
  if f == True then do
    afterFirstClick window box grid size bs f
  else do
    let newgrid = select box grid -- select box and return updated grid
    -- check if user has won
    let w = win newgrid
    if (w == True) then do
      getBody window #+ [ UI.div #+ [ string "You win!"] ] -- win message
      return ()
    else do -- if user has not won clear window and draw updated grid
      h <- getElementsByClassName window "buttonboi"
      eraseWindow window h
      draw False (alive box) bs newgrid newgrid size [] window

-- function to mark a box. box can only be marked if its not on user's first
-- click.
markSquare :: Window -> Box -> [Box] -> Int -> (Int,Int) -> Bool -> UI ()
markSquare window box grid size bs f = do
  -- if not on first click, clear window, mark box, and draw updated grid
  if f == False then do
    h <- getElementsByClassName window "buttonboi"
    eraseWindow window h
    let newgrid = markAsMine box grid []
    draw f True bs newgrid newgrid size [] window
  else
    return ()

-- to be run only on user's first click. Grid in current form should be without
-- mines. Mines are then added to grid in suhc a way to avoid selected box and
-- its neighbours. Then the selected box is cleared as normal, the window is
-- cleared and the updated grid (now with mines and boxes cleared) is drawn.
afterFirstClick :: Window -> Box -> [Box] -> Int -> (Int,Int) -> Bool -> UI ()
afterFirstClick window box grid size bs f = do
  g <- liftIO newStdGen
  let mineAmount = size * 2
  let mineIndexes = (take (mineAmount+9) . nub $ (randomRs (0,(size*size)-1) g :: [Int]))
  let ng = addMines grid mineAmount mineIndexes box
  let newgrid = select box ng
  h <- getElementsByClassName window "buttonboi"
  eraseWindow window h
  draw False True bs newgrid newgrid size [] window

-- function executed when play safe button is pressed. Passed in is the box
-- calculated to be least likely to be a mine. if no such box exists and there
-- are moves available then a random box is selected. if such a box exists then
-- it is cleared like any other.
playSafe :: Window -> Maybe Box -> [Box] -> Int -> (Int,Int) -> UI ()
-- when no move can be found based in probability
playSafe window Nothing grid size bs = do
  -- if there are moves available, ie if boxes exist that are unmarked or uncleared
  if (movesAvailable grid) == True then
    clearRandom window grid size bs
  else
    return ()
playSafe window (Just b) grid size bs =
  clearSquare window b grid size bs False

-- clears random box in grid. Random values of i and j are tried until one pair
-- gives a box that is unmarked and uncleared. that box is then cleared.
clearRandom :: Window -> [Box] -> Int -> (Int,Int) -> UI ()
clearRandom window grid size bs = do
  g1 <- liftIO newStdGen
  -- get random box
  let (i,g2) = randomR (0,size-1) g1
  let (j,_) = randomR (0,size-1) g2
  let (Box m ch r c v n) = getBox i j grid
  -- if marked or cleared try again
  if (m == True) || (ch == True) then
    clearRandom window grid size bs
  else clearSquare window (Box m ch r c v n) grid size bs False

-- gets required button size so that grid of any size has width and height
-- of half the width and height of the window
getOptimalPix :: (Int,Int) -> Int -> (Int,Int)
getOptimalPix (width,height) size = do
  let w = floor ((fromIntegral width) * (0.5))
  let h = floor ((fromIntegral height) * (0.5))
  ((w `div` size),(h `div` size))
