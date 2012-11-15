{-# LANGUAGE FlexibleInstances #-}
module Grid 
  ( Grid(..), Cell(..)
  , rows, cols, blocks
  , grid, solved
  , deadEnd
  , deadGrid
  , full, fullCell
  , numFilled
  )
where

import Data.List hiding (insert)
import qualified Data.IntSet as S
-- import Test.QuickCheck 

-- A Grid is a 9x9 matrix corresponding to a sudoku puzzle. The origin is
-- the top left corner.
newtype Grid = Grid [[Cell]]

instance Show Grid where
  show (Grid cells) = (intercalate "\n"
                       $ map (intercalate " | " . (map show)) cells) ++ "\n"

-- instance Arbitrary Grid where 
  -- arbitrary = fmap Grid (nine (nine arbitrary)) 
     -- where nine a = sequence $ take 9 $ repeat a 
    -- Keeps cells in [1..9] but produces invalid grids.
-- A Cell represents a filled or empty cell in a Grid.
newtype Cell = Cell (Maybe Int) deriving (Eq)

instance Show Cell where
  show (Cell (Just x)) = show x
  show (Cell Nothing) = "-"

-- instance Arbitrary Cell where 
  -- arbitrary = do 
    -- x <- choose (1, 9) 
    -- return $ Cell $ Just x 


-- | Tell if a grid is completely full
full :: Grid -> Bool
full g = all fullRow (rows g)
    where fullRow = all fullCell


-- | Tell if a cell is full
fullCell :: Cell -> Bool
fullCell (Cell (Just _)) = True
fullCell (Cell Nothing)  = False



-- Returns a grid with all cells empty
emptyGrid :: Grid
emptyGrid = Grid $ take 9 $ repeat $ take 9 $ repeat (Cell Nothing)

-- Returns a grid given a list of strings.
-- Characters that are "-" are treated as Nothing values.
-- Characters that are Ints in [1, 9] are treated as Just x values.
grid :: [String] -> Grid
grid cells = Grid $ map (map maybeify . filter (/= ' ')) cells
  where maybeify :: Char -> Cell
        maybeify '-' = Cell Nothing
        maybeify x = Cell (Just $ read [x])

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

-- | @deadGrid@ tells if a grid cannot possibly be completed into a solution

deadGrid :: Grid -> Bool
deadGrid = not . valid

valid = pvalid

-- | @valid@ returns true if and only if all rows, columns and blocks  contain
-- either a Nothing value or a unique Just x value where x is in [1, 9]
pvalid :: Grid -> Bool
pvalid grid = v (rows grid) && v (cols grid) && v (blocks grid)
  where v = all valid'

valid' :: [Cell] -> Bool
valid' cells = valid'' cells S.empty
  where -- valid'' :: [Cell] -> S.Set Int -> Bool
        valid'' [] _ = True
        valid'' ((Cell Nothing):xs) seen = valid'' xs seen
        valid'' (Cell (Just x):xs) seen =
          not (x `S.member` seen) && valid'' xs (x `S.insert` seen)

class MySet a where
  empty :: a
  member :: Int -> a -> Bool
  insert :: Int -> a -> a

-- myempty :: S.Set Int 
-- myempty = S.empty 
--  
-- instance MySet (S.Set Int) where 
  -- empty = S.empty 
  -- member = S.member 
  -- insert = S.insert 

inRange :: [Cell] -> Bool
inRange = all inRangeCell
  where inRangeCell (Cell Nothing) = False
        inRangeCell (Cell (Just x)) = x >= 1 && x <= 9 

deadEnd :: [Cell] -> Bool
deadEnd = not . valid'


numFilled :: Grid -> Int
numFilled grid = sum (map row (rows grid))
    where row = length . filter fullCell
                 
                 



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
