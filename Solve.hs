module Solve
  (
  )
where

import Grid

extend :: Grid -> [Grid]
extend = error "extend not implemented"


improve :: [Grid] -> [Grid]
improve gs = if any full gs then filter full gs
             else (improve . filter (not . deadGrid) . concat . map extend) gs


--------------------------
ep grid = all bigger (extend grid)
    where bigger g' = numFilled g' == numFilled grid + 1

fp grid = full grid == all (all fullCell) (rows grid)

sndp g = if solvable g then not (deadGrid g) else True

solvable g = True -- too hard to compute


