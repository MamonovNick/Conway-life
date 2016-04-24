module Config where

import Graphics.Gloss
import Types

--Config Simulation
numSimStep :: Int
numSimStep = 60

-- Size of window
hWin :: Int
hWin = 600

wWin :: Int
wWin = 800

-- Param of bar
barXsize :: Int
barXsize = 350

barYsize :: Int
barYsize = 60

barXoff :: Int
barXoff =  0

barYoff :: Int
barYoff = - hWin `div` 2 + barYsize `div` 2 + 5

-- Param of field
cellXsize :: Int
cellXsize = 8

cellYsize :: Int
cellYsize = 8

fieldXoff :: Int
fieldXoff = - cellXsize * colNum `div` 2

fieldYoff :: Int
fieldYoff = cellYsize * rowNum `div` 2

-- Number of row
rowNum :: Int
rowNum = 50

--Number of col
colNum :: Int
colNum = 50

-- Colors of things
backgroundColor   = greyN 0.2
deadColor	  = black
livingColor	  = white
testColor         = makeColor 0.8 0.2 0.2 0.4


