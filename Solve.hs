module Solve
  (
  )
where

import Grid



improve :: [Grid] -> [Grid]
improve gs = if any full gs then filter full gs
             else (improve . filter (not . deadGrid) . concat . map extend) gs


extend :: Grid -> [Grid]
extend (Grid rows) = map Grid (extendRows rows)
    where extendRows (row:rows) =
              case extendRow row of
                [] -> map (row:) (extendRows rows)
                rows' -> [row:rows | row <- rows']
          extendRows [] = error "tried to extend a full grid"

          extendRow (Cell Nothing : cells) = [Cell (Just n) : cells | n <- [1..9]]
          extendRow (c : cells) = [c : cells' | cells' <- extendRow cells]



--------------------------
ep grid = all bigger (extend grid)
    where bigger g' = numFilled g' == numFilled grid + 1

fp grid = full grid == all (all fullCell) (rows grid)

sndp g = if solvable g then not (deadGrid g) else True

solvable g = True -- too hard to compute


