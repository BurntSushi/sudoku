module Grid where

import Data.List
import qualified Data.Set as S
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- A Grid is a 9x9 matrix corresponding to a sudoku puzzle. The origin is
-- the top left corner.
newtype Grid = Grid [[Cell]]

instance Show Grid where
  show (Grid cells) = (intercalate "\n"
                       $ map (intercalate " | " . (map show)) cells) ++ "\n"

instance Arbitrary Grid where
  arbitrary =
    -- Keeps cells in [1..9] but produces invalid grids.
    fmap Grid $ sequence $ take 9 $ repeat $ vectorOf 9 (arbitrary :: Gen Cell)

-- A Cell represents a filled or empty cell in a Grid.
newtype Cell = Cell (Maybe Int) deriving (Eq)

instance Show Cell where
  show (Cell (Just x)) = show x
  show (Cell Nothing) = "-"

instance Arbitrary Cell where
  arbitrary = do
    x <- choose (1, 9)
    return $ Cell $ Just x

-- Returns a grid with all cells empty
emptyGrid :: Grid
emptyGrid = Grid $ take 9 $ repeat $ take 9 $ repeat (Cell Nothing)

-- Returns a grid given a list of list of ints.
-- Ints that are 0 are treated as Nothing values.
-- Ints that are in [1, 9] are treated as Just x values.
grid :: [[Int]] -> Grid
grid cells = Grid $ map (map maybeify) cells
  where maybeify :: Int -> Cell
        maybeify 0 = Cell Nothing
        maybeify x = Cell (Just x)

-- solved returns true if and only if the grid is valid and all rows, columns 
-- and blocks sum to 9.
solved :: Grid -> Bool
solved g = valid g && all sum45 bs && all sum45 rs && all sum45 cs
  where bs = blocks g
        rs = rows g
        cs = cols g

-- sum45 returns true when the sum of cells equals 45.
-- If a Nothing value is in cells, sum45 returns false.
sum45 :: [Cell] -> Bool
sum45 cells = sum45' cells 0
  where sum45' [] sum = sum == 45
        sum45' ((Cell Nothing):_) _ = False
        sum45' (Cell (Just c):cs) sum = sum45' cs (sum + c)

-- valid returns true if and only if all rows, columns and blocks  contain
-- either a Nothing value or a unique Just x value where x is in [1, 9]
valid :: Grid -> Bool
valid grid = v (blocks grid) && v (rows grid) && v (cols grid)
  where v = all valid'

valid' :: [Cell] -> Bool
valid' cells = valid'' cells S.empty
  where valid'' :: [Cell] -> S.Set Int -> Bool
        valid'' [] _ = True
        valid'' ((Cell Nothing):xs) seen = valid'' xs seen
        valid'' (Cell (Just x):xs) seen =
          if x `S.member` seen then
            False
          else
            valid'' xs (x `S.insert` seen)

-- Returns all the rows in the grid
rows :: Grid -> [[Cell]]
rows (Grid cells) = cells

-- Returns the row at i (indexed 1 through 9)
row :: Grid -> Int -> [Cell]
row (Grid cells) i = head $ drop (i - 1) cells

-- Returns all the columns in the grid.
cols :: Grid -> [[Cell]]
cols g = foldr (\index accum -> (col g index):accum) [] [1..9]

-- Returns the column at i (indexed 1 through 9)
col :: Grid -> Int -> [Cell]
col (Grid cells) i = foldr (\r c -> (r !! (i - 1)):c) [] cells

-- Returns all of the blocks in the grid.
blocks :: Grid -> [[Cell]]
blocks g = foldr (\index accum -> (block g index):accum) [] [1..9]

-- Returns the block at i (index 1 through 9, left to right then top to bottom)
block :: Grid -> Int -> [Cell]
block g i =
  -- I am ashamed of myself.
  case i of
    1 -> blockify [0..2] [0..2]
    2 -> blockify [0..2] [3..5]
    3 -> blockify [0..2] [6..8]
    4 -> blockify [3..5] [0..2]
    5 -> blockify [3..5] [3..5]
    6 -> blockify [3..5] [6..8]
    7 -> blockify [6..8] [0..2]
    8 -> blockify [6..8] [3..5]
    9 -> blockify [6..8] [6..8]
  where blockify :: [Int] -> [Int] -> [Cell]
        blockify rs cs = foldr (++) [] (map onlyCols rows)
          where onlyCols :: [Cell] -> [Cell]
                onlyCols row = foldr (\ind accum -> (row !! ind):accum) [] cs

                rows :: [[Cell]]
                rows = map (row g) $ map (+ 1) rs
