module Game_graphics where

import Config
import Types
import Game_logic

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe

start_life :: IO ()
start_life = play window backgroundColor numSimStep initWorld worldToPic event_handler simIter

window :: Display
window = InWindow
        "Conway life"   -- window text
        (wWin, hWin)      -- window size
        (100, 50)      -- window offset

initWorld :: LifeWorld
initWorld = (fillCellList, (True, 10, 10), (True, 10, 10))

worldToPic :: LifeWorld -> Picture
worldToPic w = Pictures (makeSc w)

makeSc :: LifeWorld -> [Picture]
makeSc (cells, ctrl', _) = (drawField cells) ++ (drawCtrlbar ctrl')

drawField :: AliveCells -> [Picture]
drawField cells = drawR rowNum cells

drawR :: Int -> AliveCells -> [Picture]
drawR 0 _ = []
drawR n cells = (drawC colNum (rowNum - n) cells) ++ (drawR (n - 1) cells)

drawC :: Int -> Int -> AliveCells -> [Picture]
drawC 0 _ _ = []
drawC n rN cells
        | (rN, (colNum - n)) `elem` cells = (Translate (fromIntegral (fieldXoff + (colNum - n) * cellXsize)) (fromIntegral (fieldYoff - rN * cellYsize)) dCell):(drawC (n - 1) rN cells)
        | otherwise = (Translate (fromIntegral (fieldXoff + (colNum - n) * cellXsize)) (fromIntegral (fieldYoff - rN * cellYsize)) aCell):(drawC (n - 1) rN cells)

aCell :: Picture
aCell = Pictures
        [ Color white   (rectangleSolid (fromIntegral cellXsize) (fromIntegral cellYsize))
        , Color black   (rectangleWire  (fromIntegral cellXsize) (fromIntegral cellYsize)) ]

dCell :: Picture
dCell = Pictures
        [ Color black   (rectangleSolid (fromIntegral cellXsize) (fromIntegral cellYsize))]

drawCtrlbar :: Control -> [Picture]
drawCtrlbar (b, _, _) = (Translate (fromIntegral barXoff) (fromIntegral barYoff) dBar):(drawPlay b):(drawStop):(drawSpUp):(drawSpDn):(drawFast):[]

dBar :: Picture
dBar = Pictures
        [ Color (greyN 0.4)     (rectangleSolid (fromIntegral barXsize) (fromIntegral barYsize))
        , Color black   (rectangleWire  (fromIntegral barXsize) (fromIntegral barYsize)) ]

-------draw buttons----------
drawPlay :: Bool -> Picture
drawPlay True = Translate (fromIntegral barXoff + 35) (fromIntegral barYoff)
             $ Scale 0.35 0.35
             $ unsafePerformIO (loadBMP "data/playhot.bmp")
drawPlay False = Translate (fromIntegral barXoff + 35) (fromIntegral barYoff)
             $ Scale 0.35 0.35
             $ unsafePerformIO (loadBMP "data/pause.bmp")

drawStop :: Picture
drawStop = Translate (fromIntegral barXoff - 35) (fromIntegral barYoff)
        $ Scale 0.35 0.35
        $ unsafePerformIO (loadBMP "data/stophot.bmp")

drawSpUp :: Picture
drawSpUp = Translate (fromIntegral barXoff + 105) (fromIntegral barYoff)
        $ Scale 0.35 0.35
        $ unsafePerformIO (loadBMP "data/stepfwd.bmp")

drawSpDn :: Picture
drawSpDn = Translate (fromIntegral barXoff - 105) (fromIntegral barYoff)
        $ Scale 0.35 0.35
        $ Rotate 180
        $ unsafePerformIO (loadBMP "data/stepfwd.bmp")

drawFast :: Picture
drawFast = Translate (fromIntegral barXoff + 150) (fromIntegral barYoff)
        $ Scale 0.95 0.95
        $ Rotate 180
        $ unsafePerformIO (loadBMP "data/fastf.bmp")
-----------------------------
event_handler :: Event -> LifeWorld -> LifeWorld
event_handler (EventKey (SpecialKey KeySpace) Down _ _) (w, (b, i, m), q) | b == True = (w, (False, i, m), q)
                                                                          | otherwise = (w, (True, i, m), q)
event_handler (EventKey (MouseButton LeftButton) Down _ (x, y)) (w, (b, i, m), (b1, i1, m1)) | (x >= (fromIntegral barXoff + 12)) && (x <= (fromIntegral barXoff + 58)) && (y >= (fromIntegral barYoff - 23)) && (y <= (fromIntegral barYoff + 23)) && (b == True) = (w, (False, i, m), (b1, i1, m1))
                                                                               |(x >= (fromIntegral barXoff + 12)) && (x<= (fromIntegral barXoff + 58)) && (y >= (fromIntegral barYoff - 23)) && (y <= (fromIntegral barYoff + 23)) && (b == False) = (w, (True, i, m), (b1, i1, m1))
                                                                               | (x >= (fromIntegral barXoff - 58)) && (x <= (fromIntegral barXoff - 12)) && (y >= (fromIntegral barYoff - 23)) && (y <= (fromIntegral barYoff + 23)) = initWorld
                                                                               |(x >= (fromIntegral barXoff + 82)) && (x<= (fromIntegral barXoff + 128)) && (y >= (fromIntegral barYoff - 23)) && (y <= (fromIntegral barYoff + 23)) && (m > 1) = (w, (b, i, m - 2), (b1, i1, m1))
                                                                               |(x >= (fromIntegral barXoff + 82)) && (x<= (fromIntegral barXoff + 128)) && (y >= (fromIntegral barYoff - 23)) && (y <= (fromIntegral barYoff + 23)) && (m <= 1) = (w, (b, i, 0), (b1, i1, m1))
                                                                               |(x >= (fromIntegral barXoff - 128)) && (x<= (fromIntegral barXoff - 82)) && (y >= (fromIntegral barYoff - 23)) && (y <= (fromIntegral barYoff + 23)) && (m <= numSimStep) = (w, (b, i, m + 2), (b1, i1, m1))
                                                                               |(x >= (fromIntegral barXoff - 128)) && (x<= (fromIntegral barXoff - 82)) && (y >= (fromIntegral barYoff - 23)) && (y <= (fromIntegral barYoff + 23)) && (m > numSimStep) = (w, (b, i, numSimStep), (b1, i1, m1))
                                                                               |(x >= (fromIntegral barXoff + 134)) && (x<= (fromIntegral barXoff + 166)) && (y >= (fromIntegral barYoff - 16)) && (y <= (fromIntegral barYoff + 16)) = (w, (True, 1, 1), (b, i, m))
                                                                               | otherwise = (w, (b, i, m), (b1, i1, m1))
event_handler (EventKey (MouseButton LeftButton) Up _ (x, y)) (w, (b, i, m), (b1, i1, m1)) |(x >= (fromIntegral barXoff + 134)) && (x<= (fromIntegral barXoff + 166)) && (y >= (fromIntegral barYoff - 16)) && (y <= (fromIntegral barYoff + 16)) = (w, (b1, i1, m1), (b, i, m))
                                                                               | otherwise = (w, (b, i, m), (b1, i1, m1))
event_handler _ w = w
-----------------------------
simIter :: Float -> LifeWorld -> LifeWorld
simIter _ (w, (False, i, m), q) = (w, (False, i, m), q)
simIter _ (w, (_, i, m), q) | i == 0 = (lifeStep w, (True, m, m), q)
                            | otherwise = (w, (True, i - 1, m), q)



















