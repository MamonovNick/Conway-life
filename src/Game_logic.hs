module Game_logic(fillCellList, lifeStep) where

import Types
import Config
import Data.List
import System.IO.Unsafe
import Prelude hiding (catch)
import Control.Exception
import System.Environment
import System.IO.Error (isDoesNotExistError)

fillCellList :: AliveCells
--fillCellList = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2), (18, 0), (18, 1), (18, 2)]--[(3, 5), (4, 5), (5, 5), (6, 5)]
fillCellList = unsafePerformIO (fH `catch` handler)
--fillCellList = operate "0 0 1 1"

handler :: IOException -> IO (AliveCells)
handler e = return ([(1, 0), (2, 1), (0, 2), (1, 2), (2, 2), (18, 0), (18, 1), (18, 2)])

fH :: IO(AliveCells)
fH = do
        src <- readFile "file.in"
        return (operate src)

operate :: String -> AliveCells
operate str = zip (odd1 m) (even1 m)
        where m = charToInt str 0 False

odd1 :: [Int] -> [Int]
odd1 [] = []
odd1 (x:[]) = [x]
odd1 (x:y:xs) = x:(odd1 xs)

even1 :: [Int] -> [Int]
even1 [] = []
even1 (x:[]) = []
even1 (x:y:xs) = y:(even1 xs)

charToInt :: String -> Int -> Bool -> [Int]
charToInt [] s _ = [s]
charToInt (x:xs) s b
                | (x == ' ') && (b == False) = charToInt xs 0 False
                | (x == ' ') =  s:(charToInt xs 0 False)
                | (x == '\n') && (b == False) = charToInt xs 0 False
                | (x == '\n') =  s:(charToInt xs 0 False)
                | otherwise = charToInt xs (s * 10 + ((fromEnum x) - (fromEnum '0'))) True

lifeStep :: AliveCells -> AliveCells
lifeStep cells = [head g | g <- makeSurround cells, checkSurround g cells]

checkSurround :: AliveCells -> AliveCells -> Bool
checkSurround [_, _, _] _ = True
checkSurround [x, _] cells = x `elem` cells
checkSurround _ _ = False

makeSurround :: AliveCells -> [AliveCells]
makeSurround = group . sort . concatMap surrounding

surrounding :: (Int, Int) -> AliveCells
surrounding (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0), x+dx <= (colNum - 1), x+dx >= 0, y+dy <= (rowNum - 1), y+dy >= 0]

