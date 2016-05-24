module Config where

import Graphics.Gloss

--Config Simulation
numSimStep :: Int
numSimStep = 120

-- | Heigth of window
hWin :: Int
hWin = 700

-- | Width of window
wWin :: Int
wWin = 1050

-- | Offset of menu button X coord
butXoff :: Float
butXoff = -436

-- | Offset of menu button Y coord
butYoff :: Float
butYoff = 227

-- | offset for txt of menu button X coord
txtXoff :: Int
txtXoff = -500

-- | offset for txt of menu button Y coord
txtYoff :: Int
txtYoff = 220

-- | Size of bar X coord
barXsize :: Int
barXsize = 350

-- | Size of bar Y coord
barYsize :: Int
barYsize = 60

-- | Offset of bar X coord
barXoff :: Float
barXoff =  -10

-- | Offset of bar Y coord
barYoff :: Float
barYoff = - (fromIntegral hWin) / 2 + (fromIntegral barYsize) / 2 + 5

-- | size of cell X coord
cellXsize :: Int
cellXsize = 8

-- | size of cell Y coord
cellYsize :: Int
cellYsize = 8

-- | offset of field X coord
fieldXoff :: Int
fieldXoff = - cellXsize * colNum `div` 2

-- | offset of field Y coord
fieldYoff :: Int
fieldYoff = cellYsize * rowNum `div` 2

-- | Number of row 
rowNum :: Int
rowNum = 60

-- | Number of col
colNum :: Int
colNum = 80

-- | Background color of main window
backgroundColor :: Color
backgroundColor   = greyN 0.2

