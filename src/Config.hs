module Config where

import Graphics.Gloss

--Config Simulation
numSimStep :: Int
numSimStep = 120

-- Size of window
hWin :: Int
hWin = 700

wWin :: Int
wWin = 1050

--Param of menu
butXoff :: Float
butXoff = -436

butYoff :: Float
butYoff = 227

txtXoff :: Int
txtXoff = -500

txtYoff :: Int
txtYoff = 220

-- Param of bar
barXsize :: Int
barXsize = 350

barYsize :: Int
barYsize = 60

barXoff :: Float
barXoff =  -10

barYoff :: Float
barYoff = - (fromIntegral hWin) / 2 + (fromIntegral barYsize) / 2 + 5

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
rowNum = 60

--Number of col
colNum :: Int
colNum = 80

-- Colors of things
backgroundColor :: Color
backgroundColor   = greyN 0.2

