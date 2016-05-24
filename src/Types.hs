module Types where

-- | coords of cell
type Coords = (Int, Int)

-- | list of alive cells
type AliveCells = [Coords]

-- | rectangle for proc button click
type Rect = (Float, Float, Float, Float)

-- | Second Control for backup current config
data LifeWorld = LifeWorld { aliveCells :: AliveCells
                           , ctrlM :: Control -- ^ main control of the simulation
                           , ctrlA :: Control -- ^ backup control for simulation
                           , menu :: Menu -- ^ settings for menu                          
                           }

-- | tune simulation
data Control = Control { state :: Bool -- ^ play or pause
                       , time :: Int -- ^ real time
                       , maxTime :: Int -- ^ max time
                       }

-- | menu for editing scene
data Menu = Menu { activeMenu :: Bool -- ^ editing mode is activated
                 , activeClick :: Bool -- ^ editing of field is activated
                 , lastCell :: Coords -- ^ coords of last painted cell                 
                 }

