# Haskell-Minesweeper

This is a browser-based implementation of the game Minesweeper in Haskell. The
file "Minesweeper.hs" is the back-end functionality, and the file
"MineSweeperUI.hs" is the front-end, which utilizes the Threepenny library.

# Back-End

A box is represented by the data type Box, which contains a box's value (is it a
mine or not), whether or not the user has cleared or checked it, it's i and j
position in the grid, and the list of the positions of the box's neighbours in
the grid. A grid is represented as a one-dimensional list of Box's. While a 2d
list might have been better to easily represented rows and columns, a 1d list
was chosen because of the ease in traversing it.

Like the size of the (square) grid, the number of mines in the grid is
hard-coded. The way this implementation of the game is designed, the first box
selected by the user to clear cannot be a mine, nor can its neighbours.
Therefore, the grid is initially without mines, and the mines are randomly set
after the first user selection in such a way that avoids the selected box and
its neighbours.

This is done by generating a random list of indices of potential boxes to be set
as mines. This list is 9 greater than the actual number of mines needed, to
account for the potential of the selected box and all of its neighbours
being included in the random list. If any of these are present they are removed,
and then the necessary number of mines is taken from the rest of the list. The
boxes corresponding to the remaining indices are set as mines.

A user dies when they select a mine. A user wins when all non-mines have been
cleared.

When a non-mine is selected and it has no neighbouring mines then it and all of
its immediate neighbours are cleared. If any of its neighbours have no
neighbouring mines then all of its neighbours are cleared, and so on. Clearing
propagation stops when it encounters a neighbour with at least one neighbouring
mine. If a selected box has at least one neighbouring mine then only the box is
cleared, and none of its neighbours.

A major feature in the game is the AI player, an option the user can take that
will attempt to make the safest possible move available based on the probability
that each unvisited box is a mine.

The AI player can only see the board as the user sees it. It starts by
generating a list of all the available boxes for which it can calculate a
probability and a corresponding score for each box, and then takes the box
with the lowest score. If there are no boxes that can be reasoned about then
the AI Player will select a random uncleared and unmarked box.

A score is a sum of probabilities. The AI player visits every cleared box in the
grid, and assigns a probability to each of its unchecked neighbours that
depends on how many neighbouring mines the cleared box has, how many of its
neighbours have been cleared, and how many have been marked as potential mines
by the user. If an uncleared neighbour is already in the list of probabilities
then its new probability is added to its score. Otherwise its added to the list
with its score initially set to the new probability.

# Front-End

The front end browser functionality is executed using the Threepenny library.
Each Box in the grid is mapped onto a button. The appearance of a button depends
on the features of the Box it represents. For example any uncleared Box is a
plain grey button. Any cleared non-mine is a green button that also displays the
number of neighbouring mines. Any marked uncleared box is blue. Any cleared mine
is red.

Because lists are immutable, whenever a box is selected or marked a new grid has
to be created that represents the changes made. Therefore after each move by the
user the grid has to be redrawn on the front-end, i.e each box has to be mapped
again onto a button.

After the first click the user is presented with a button that allows them to
play the safest possible move determined by the AI Player.

![alt text](/images/lost.png)

![alt text](/images/won/png)
