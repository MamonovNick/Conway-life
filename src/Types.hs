module Types where

type Coords = (Int, Int)

type AliveCells = [Coords]

type Rect = (Float, Float, Float, Float)

-- Second Control for backup current config
data LifeWorld = LifeWorld { aliveCells :: AliveCells
                           , ctrlM :: Control
                           , ctrlA :: Control
                           , menu :: Menu
                           }

data Control = Control { state :: Bool
                       , time :: Int
                       , maxTime :: Int
                       }

data Menu = Menu { activeMenu :: Bool
                 , activeClick :: Bool
                 , lastCell :: Coords
                 }

