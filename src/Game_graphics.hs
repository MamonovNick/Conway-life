module Game_graphics where

import Config
import Types
import Game_logic
import Data.List

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe

start_life :: IO ()
start_life = play window backgroundColor numSimStep initWorld worldToPic event_handler simIter

window :: Display
window = InWindow
        "Conway life"
        (wWin, hWin)
        (100, 50)

initWorld :: LifeWorld
initWorld = (LifeWorld fillCellList (Control True 10 10) (Control True 10 10) (Menu False False (0, 0)))

worldToPic :: LifeWorld -> Picture
worldToPic world = Pictures (makeScene world)

makeScene :: LifeWorld -> [Picture]
makeScene (LifeWorld aC cM _ _) = (drawField aC) ++ (drawCtrlbar cM)

----draw field---------------
drawField :: AliveCells -> [Picture]
drawField cells = drawR rowNum cells

--draw rows
drawR :: Int -> AliveCells -> [Picture]
drawR 0 _ = []
drawR n cells = (drawC colNum (rowNum - n) cells) ++ (drawR (n - 1) cells)

--draw cells in row
drawC :: Int -> Int -> AliveCells -> [Picture]
drawC 0 _ _ = []
drawC n rN cells
        | (rN, (colNum - n)) `elem` cells = (Translate (fromIntegral (fieldXoff + (colNum - n) * cellXsize)) (fromIntegral (fieldYoff - rN * cellYsize)) dCell):(drawC (n - 1) rN cells)
        | otherwise = (Translate (fromIntegral (fieldXoff + (colNum - n) * cellXsize)) (fromIntegral (fieldYoff - rN * cellYsize)) aCell):(drawC (n - 1) rN cells)

--draw alive cell
aCell :: Picture
aCell = Pictures
        [ Color white   (rectangleSolid (fromIntegral cellXsize) (fromIntegral cellYsize))
        , Color (greyN 0.65)   (rectangleWire  (fromIntegral cellXsize) (fromIntegral cellYsize)) ]

--draw dead cell
dCell :: Picture
dCell = Pictures
        [ Color black   (rectangleSolid (fromIntegral cellXsize) (fromIntegral cellYsize))
        , Color (greyN 0.65)   (rectangleWire  (fromIntegral cellXsize) (fromIntegral cellYsize)) ]
-----------------------------
----draw Control bar---------
drawCtrlbar :: Control -> [Picture]
drawCtrlbar (Control st _ _) = (Translate barXoff barYoff dBar):(drawPlay st):(drawStop):(drawSpUp):(drawSpDn):(drawFast):(drawScEdit):(drawTxt):[]

dBar :: Picture
dBar = Pictures
        [ Color (greyN 0.4)     (rectangleSolid (fromIntegral barXsize) (fromIntegral barYsize))
        , Color black   (rectangleWire  (fromIntegral barXsize) (fromIntegral barYsize)) ]

-------draw buttons----------
drawPlay :: Bool -> Picture
drawPlay True = Translate (barXoff + 35) barYoff
             $ Scale 0.35 0.35
             $ unsafePerformIO (loadBMP "data/playhot.bmp")
drawPlay False = Translate (barXoff + 35) barYoff
             $ Scale 0.35 0.35
             $ unsafePerformIO (loadBMP "data/pause.bmp")

drawStop :: Picture
drawStop = Translate (barXoff - 35) barYoff
        $ Scale 0.35 0.35
        $ unsafePerformIO (loadBMP "data/stophot.bmp")

drawSpUp :: Picture
drawSpUp = Translate (barXoff + 105) barYoff
        $ Scale 0.35 0.35
        $ unsafePerformIO (loadBMP "data/stepfwd.bmp")

drawSpDn :: Picture
drawSpDn = Translate (barXoff - 105) barYoff
        $ Scale 0.35 0.35
        $ Rotate 180
        $ unsafePerformIO (loadBMP "data/stepfwd.bmp")

drawFast :: Picture
drawFast = Translate (barXoff + 150) barYoff
        $ Scale 0.95 0.95
        $ Rotate 180
        $ unsafePerformIO (loadBMP "data/fastf.bmp")

drawScEdit :: Picture
drawScEdit = Translate butXoff butYoff
         $ Pictures [Color (red)   (rectangleWire  140 30)]

drawTxt :: Picture
drawTxt = Translate (fromIntegral txtXoff) (fromIntegral txtYoff)
        $ Color red
        $ Scale 0.15 0.15
        $ Text "Scene Editor"
-----------------------------

buttons :: [(Rect, Point -> LifeWorld -> LifeWorld, KeyState)]
buttons =
        [ ((  12 + barXoff,  58 + barXoff, -23 + barYoff, 23 + barYoff), btnPlay, Down)
        , (( -58 + barXoff, -12 + barXoff, -23 + barYoff, 23 + barYoff), btnStop, Down)
        , ((  82 + barXoff, 128 + barXoff, -23 + barYoff, 23 + barYoff), btnSpeedUp, Down)
        , ((-128 + barXoff, -82 + barXoff, -23 + barYoff, 23 + barYoff), btnSpeedDown, Down)
        , (( 134 + barXoff, 166 + barXoff, -16 + barYoff, 16 + barYoff), btnAccelUp, Up)
        , (( 134 + barXoff, 166 + barXoff, -16 + barYoff, 16 + barYoff), btnAccelDown, Down)
        , (( -70 + butXoff,  70 + butXoff, -15 + butYoff, 15 + butYoff), btnEdit, Down)
        , ((fromIntegral (fieldXoff - 1), fromIntegral (-fieldXoff), fromIntegral (-fieldYoff), fromIntegral (fieldYoff)), fieldPaintD, Down)
        , ((fromIntegral (fieldXoff - 1), fromIntegral (-fieldXoff), fromIntegral (-fieldYoff), fromIntegral (fieldYoff)), fieldPaintU, Up)
        ]

btnPlay :: Point -> LifeWorld -> LifeWorld
btnPlay _ w@LifeWorld{ctrlM=c@Control{state=b}, menu=Menu{activeMenu=am}}
        | am = w
        | otherwise = w{ctrlM=c{state = not b}}

btnStop :: Point -> LifeWorld -> LifeWorld
btnStop _ w@LifeWorld{menu=Menu{activeMenu=am}}
        | am = w
        | otherwise = initWorld

btnSpeedUp :: Point -> LifeWorld -> LifeWorld
btnSpeedUp _ w@LifeWorld{ctrlM=c@Control{maxTime=m}, menu=Menu{activeMenu=am}}
        | am = w
        | m > 1 = w{ctrlM=c{time =  m - 2, maxTime = m - 2}}
        | otherwise = w{ctrlM=c{time =  0, maxTime = 0}}

btnSpeedDown :: Point -> LifeWorld -> LifeWorld
btnSpeedDown _ w@LifeWorld{ctrlM=c@Control{maxTime=m}, menu=Menu{activeMenu=am}}
        | am = w
        | m < numSimStep-1 = w{ctrlM=c{time = m+2, maxTime = m+2}}
        | otherwise = w{ctrlM=c{time = numSimStep, maxTime = numSimStep}}

btnAccelUp :: Point -> LifeWorld -> LifeWorld
btnAccelUp _ w@LifeWorld{ctrlM=c@Control{}, ctrlA=Control{state=s,time=t, maxTime=m}, menu=Menu{activeMenu=am}} 
        | am = w
        | otherwise = w{ctrlM=c{state=s,time=t, maxTime=m}}

btnAccelDown :: Point -> LifeWorld -> LifeWorld
btnAccelDown _ w@LifeWorld{ctrlM=c@Control{state=s,time=t, maxTime=m}, ctrlA=d@Control{}, menu=Menu{activeMenu=am}} 
        | am = w
        | otherwise = w{ctrlM=c{state=True, time=1, maxTime=1}, ctrlA=d{state=s,time=t, maxTime=m}}

btnEdit :: Point -> LifeWorld -> LifeWorld
btnEdit _ w@LifeWorld{ctrlM=c@Control{state=s}, ctrlA=a@Control{state=sb}, menu=m@Menu{activeMenu=am}}
        | not am = w{ctrlM=c{state=False}, ctrlA=a{state=s}, menu=m{activeMenu=True}}
        | otherwise = w{ctrlM=c{state=sb}, menu=m{activeMenu=False}}

fieldPaintD :: Point -> LifeWorld -> LifeWorld
fieldPaintD (x, y) w@LifeWorld{aliveCells = a, menu=m@Menu{activeMenu=am, lastCell=lc}}
        | (not am) || ((fstCrd, sndCrd) == lc) = w
        | otherwise = w{aliveCells=(changeCell (fstCrd, sndCrd) a), menu=m{activeClick=True, lastCell=(fstCrd,sndCrd)}}
        where fstCrd = rowNum `div` 2 - (truncate y + cellYsize `div` 2) `div` cellYsize
              sndCrd = (truncate x + cellXsize `div` 2) `div` cellXsize + colNum `div` 2

fieldPaintU :: Point -> LifeWorld -> LifeWorld
fieldPaintU _ w@LifeWorld{menu=m@Menu{activeMenu=am}}
        | not am = w
        | otherwise = w{menu=m{activeClick=False}}

changeCell :: Coords -> AliveCells -> AliveCells
changeCell (x, y) [] = [(x, y)]
changeCell (x, y) (a:xs) = if a /= (x, y) then a:(changeCell (x, y) xs) else xs

rectClick :: Point -> Rect -> Bool
rectClick (x, y) (x1, x2, y1, y2) = (x >=  x1) && (x <= x2) && (y >= y1) && (y <= y2)

checkButton :: Point -> KeyState -> (Rect, Point -> LifeWorld -> LifeWorld, KeyState) -> Bool
checkButton (x, y) keyState (rect, _, btnKeyState) = if keyState /= btnKeyState then False else rectClick (x, y) rect

------handling events--------
event_handler :: Event -> LifeWorld -> LifeWorld
event_handler (EventKey (SpecialKey KeySpace) Down _ _) w@LifeWorld{ctrlM=c@Control{state=b}, menu=Menu{activeMenu=a}}
        | a = w
        | otherwise = w{ctrlM=c{state = not b}}
event_handler (EventKey (MouseButton LeftButton) kSt _ (x, y)) w
         = case find (checkButton (x, y) kSt) buttons of
                Just (_, action, _) -> action (x, y) w
                Nothing -> w
event_handler (EventMotion (x, y)) w@LifeWorld{menu=Menu{activeMenu=am, activeClick=ac}}
        | am && ac = (fieldPaintD (x, y) w)
        | otherwise = w
event_handler _ w = w
-----------------------------

--Step world one iteration---
simIter :: Float -> LifeWorld -> LifeWorld
simIter _ w@LifeWorld{ctrlM=c@Control{state=False}} = w{ctrlM = c{state=False}}
simIter _ w@LifeWorld{aliveCells = a, ctrlM=c@Control{time=i, maxTime=m}} 
        | i == 0 =  w{aliveCells = (lifeStep a), ctrlM = c{time=m}}
        | otherwise = w{ctrlM = c{time=i-1}}
-----------------------------


















