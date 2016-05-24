module Game_graphics where

import Config
import Types
import Game_logic
import Data.List

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe

-- | starting function play
start_life :: IO ()
start_life = play window backgroundColor numSimStep initWorld worldToPic event_handler simIter

-- | create window
window :: Display
window = InWindow
        "Conway life"
        (wWin, hWin)
        (100, 50)

-- | Build init world from extr file or predownload scene
initWorld :: LifeWorld
initWorld = (LifeWorld fillCellList (Control True 10 10) (Control True 10 10) (Menu False False (500, 500)))

-- | make Picture from LifeWorld for play function
worldToPic :: LifeWorld -> Picture
worldToPic world = Pictures (makeScene world)

-- | construct interface, aliveC - list of cells, ctrlMenu - params for bar buttons
makeScene :: LifeWorld -> [Picture]
makeScene (LifeWorld aliveC ctrlMenu _ _) = (drawField aliveC) ++ (drawCtrlbar ctrlMenu)

-- | draw field
drawField :: AliveCells -> [Picture]
drawField cells = drawRows rowNum cells

-- | draw rows, Int - number of rows
drawRows :: Int -> AliveCells -> [Picture]
drawRows 0 _ = []
drawRows n cells = (drawC colNum (rowNum - n) cells) ++ (drawRows (n - 1) cells)

-- | draw cells in row, 
drawC :: Int -> Int -> AliveCells -> [Picture]
drawC n rN cells = map (\i -> drawCell i rN cells) [0..n]
      
-- | Draw a single cell
drawCell :: Int -> Int -> AliveCells -> Picture
drawCell n rN cells = Translate dx dy deadOrAliveCell
    where
      dx = fromIntegral (fieldXoff + (colNum - n) * cellXsize)
      dy = fromIntegral (fieldYoff - rN * cellYsize)
      deadOrAliveCell = if isAlive then aCell else dCell
      isAlive = (rN, colNum - n) `elem` cells

-- | draw alive cell with black color
aCell :: Picture
aCell = Pictures
        [ Color white          (rectangleSolid (fromIntegral cellXsize) (fromIntegral cellYsize))
        , Color (greyN 0.65)   (rectangleWire  (fromIntegral cellXsize) (fromIntegral cellYsize))
        ]

-- | draw dead cell with black color
dCell :: Picture
dCell = Pictures
        [ Color black          (rectangleSolid (fromIntegral cellXsize) (fromIntegral cellYsize))
        , Color (greyN 0.65)   (rectangleWire  (fromIntegral cellXsize) (fromIntegral cellYsize))
        ]

-- | draw control bar
drawCtrlbar :: Control -> [Picture]
drawCtrlbar (Control st _ _) = (Translate barXoff barYoff dBar):(drawPlay st):(drawStop):(drawSpUp):(drawSpDn):(drawFast):(drawScEdit):(drawTxt):[]

dBar :: Picture
dBar = Pictures
        [ Color (greyN 0.4)    (rectangleSolid (fromIntegral barXsize) (fromIntegral barYsize))
        , Color black          (rectangleWire  (fromIntegral barXsize) (fromIntegral barYsize))
        ]

-- | draw play button, Bool is for choosing rigth picture
drawPlay :: Bool -> Picture
drawPlay True = Translate (barXoff + 35) barYoff
             $ Scale 0.35 0.35
             $ unsafePerformIO (loadBMP "data/playhot.bmp")
drawPlay False = Translate (barXoff + 35) barYoff
             $ Scale 0.35 0.35
             $ unsafePerformIO (loadBMP "data/pause.bmp")

-- | draw stop button
drawStop :: Picture
drawStop = Translate (barXoff - 35) barYoff
        $ Scale 0.35 0.35
        $ unsafePerformIO (loadBMP "data/stophot.bmp")

-- | draw speed up button
drawSpUp :: Picture
drawSpUp = Translate (barXoff + 105) barYoff
        $ Scale 0.35 0.35
        $ unsafePerformIO (loadBMP "data/stepfwd.bmp")

-- | draw speed down button
drawSpDn :: Picture
drawSpDn = Translate (barXoff - 105) barYoff
        $ Scale 0.35 0.35
        $ Rotate 180
        $ unsafePerformIO (loadBMP "data/stepfwd.bmp")

-- | draw accelerate button
drawFast :: Picture
drawFast = Translate (barXoff + 150) barYoff
        $ Scale 0.95 0.95
        $ Rotate 180
        $ unsafePerformIO (loadBMP "data/fastf.bmp")

-- | draw rectangle for menu editor button
drawScEdit :: Picture
drawScEdit = Translate butXoff butYoff
         $ Pictures [Color (red)   (rectangleWire  140 30)]

-- | draw text for menu eeditor button
drawTxt :: Picture
drawTxt = Translate (fromIntegral txtXoff) (fromIntegral txtYoff) -- draw txt
        $ Color red
        $ Scale 0.15 0.15
        $ Text "Scene Editor"

-- | buttons location on the form
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

-- | function for processing button play/pause click
btnPlay :: Point -> LifeWorld -> LifeWorld
btnPlay _ w@LifeWorld{ctrlM=c@Control{state=b}, menu=Menu{activeMenu=am}}
        | am = w
        | otherwise = w{ctrlM=c{state = not b}}

-- | function for processing button stop click
btnStop :: Point -> LifeWorld -> LifeWorld
btnStop _ w@LifeWorld{menu=Menu{activeMenu=am}}
        | am = w
        | otherwise = initWorld

-- | function for processing button speed up click
btnSpeedUp :: Point -> LifeWorld -> LifeWorld
btnSpeedUp _ w@LifeWorld{ctrlM=c@Control{maxTime=m}, menu=Menu{activeMenu=am}}
        | am = w
        | m > 1 = w{ctrlM=c{time =  m - 2, maxTime = m - 2}}
        | otherwise = w{ctrlM=c{time =  0, maxTime = 0}}

-- | function for processing button speed down click
btnSpeedDown :: Point -> LifeWorld -> LifeWorld
btnSpeedDown _ w@LifeWorld{ctrlM=c@Control{maxTime=m}, menu=Menu{activeMenu=am}}
        | am = w
        | m < numSimStep-1 = w{ctrlM=c{time = m+2, maxTime = m+2}}
        | otherwise = w{ctrlM=c{time = numSimStep, maxTime = numSimStep}}

-- | function for processing mouse up click on acceleration button
btnAccelUp :: Point -> LifeWorld -> LifeWorld
btnAccelUp _ w@LifeWorld{ctrlM=c@Control{}, ctrlA=Control{state=s,time=t, maxTime=m}, menu=Menu{activeMenu=am}} 
        | am = w
        | otherwise = w{ctrlM=c{state=s,time=t, maxTime=m}}

-- | function for processing mouse down click on acceleration button
btnAccelDown :: Point -> LifeWorld -> LifeWorld
btnAccelDown _ w@LifeWorld{ctrlM=c@Control{state=s,time=t, maxTime=m}, ctrlA=d@Control{}, menu=Menu{activeMenu=am}} 
        | am = w
        | otherwise = w{ctrlM=c{state=True, time=1, maxTime=1}, ctrlA=d{state=s,time=t, maxTime=m}}

-- | function for processin click on the field, Point - location of the click
btnEdit :: Point -> LifeWorld -> LifeWorld
btnEdit _ w@LifeWorld{ctrlM=c@Control{state=s}, ctrlA=a@Control{state=sb}, menu=m@Menu{activeMenu=am}}
        | not am = w{ctrlM=c{state=False}, ctrlA=a{state=s}, menu=m{activeMenu=True}}
        | otherwise = w{ctrlM=c{state=sb}, menu=m{activeMenu=False}}

-- | draw cells on field in editor mode
fieldPaintD :: Point -> LifeWorld -> LifeWorld
fieldPaintD (x, y) w@LifeWorld{aliveCells = a, menu=m@Menu{activeMenu=am, lastCell=lc}}
        | (not am) || (crd == lc) = w
        | otherwise = w{
            aliveCells = changeCell crd a,
            menu = m { activeClick = True, lastCell = crd}
          }
        where
            crd = (fstCrd, sndCrd)
            fstCrd = rowNum `div` 2 - (truncate y + cellYsize `div` 2) `div` cellYsize
            sndCrd = (truncate x + cellXsize `div` 2) `div` cellXsize + colNum `div` 2
              
-- | function for processing up click on field
fieldPaintU :: Point -> LifeWorld -> LifeWorld
fieldPaintU _ w@LifeWorld{menu=m@Menu{activeMenu = am}}
        | not am = w
        | otherwise = w{menu= m{activeClick = False}}

-- | insert or delete cell in array
changeCell :: Coords -> AliveCells -> AliveCells
changeCell (x, y) [] = [(x, y)]
changeCell (x, y) (a:xs) = if a /= (x, y) then a:(changeCell (x, y) xs) else xs

-- | check rectangle hit, Point - location of click, Rect - rectangle, where buton is located
rectClick :: Point -> Rect -> Bool
rectClick (x, y) (x1, x2, y1, y2) = (x >=  x1) && (x <= x2) && (y >= y1) && (y <= y2)

-- | check hit on the button, Point - real click location, KeyState - Up or Down
checkButton :: Point -> KeyState -> (Rect, Point -> LifeWorld -> LifeWorld, KeyState) -> Bool
checkButton (x, y) keyState (rect, _, btnKeyState) = if keyState /= btnKeyState then False else rectClick (x, y) rect

-- | handling events
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

-- | Step world one iteration
simIter :: Float -> LifeWorld -> LifeWorld
simIter _ w@LifeWorld{ctrlM=c@Control{state=False}} = w{ctrlM = c{state=False}}
simIter _ w@LifeWorld{aliveCells = a, ctrlM=c@Control{time=i, maxTime=m}} 
        | i == 0 =  w{aliveCells = (lifeStep a), ctrlM = c{time=m}}
        | otherwise = w{ctrlM = c{time=i-1}}


















