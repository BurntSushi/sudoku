module Main where

import Grid
import Solve

exInvalid :: Grid
exInvalid = grid $ [ "5 3 3 - 7 - - - -"
                   , "6 - - 1 9 5 - - -"
                   , "- 9 8 - - - - 6 -"

                   , "8 - - - 6 - - - 3"
                   , "4 - - 8 - 3 - - 1"
                   , "7 - - - 2 - - - 6"

                   , "- 6 - - - - 2 8 -"
                   , "- - - 4 1 9 - - 5"
                   , "- - - - 8 - - 7 9"
                   ]

wikiGapped :: Grid
wikiGapped = grid $ [ "5 3 - - 7 - - - -"
                    , "6 - - 1 9 5 - - -"
                    , "- 9 8 - - - - 6 -"

                    , "8 - - - 6 - - - 3"
                    , "4 - - 8 - 3 - - 1"
                    , "7 - - - 2 - - - 6"

                    , "- 6 - - - - 2 8 -"
                    , "- - - 4 1 9 - - 5"
                    , "- - - - 8 - - 7 9"
                    ]

wikiAnswered :: Grid
wikiAnswered = grid $ [ "5 3 4 6 7 8 9 1 2"
                      , "6 7 2 1 9 5 3 4 8"
                      , "1 9 8 3 4 2 5 6 7"

                      , "8 5 9 7 6 1 4 2 3"
                      , "4 2 6 8 5 3 7 9 1"
                      , "7 1 3 9 2 4 8 5 6"

                      , "9 6 1 5 3 7 2 8 4"
                      , "2 8 7 4 1 9 6 3 5"
                      , "3 4 5 2 8 6 1 7 9"
                      ]

main = putStr $
       "Here's our puzzle. Has it been solved?\n"
       ++ show wikiGapped
       ++ solvedStr wikiGapped
       ++ "\n\n"
       ++ "Is the answer from Wikipedia correct?\n"
       ++ show wikiAnswered
       ++ solvedStr wikiAnswered
       ++ "\n\n"
       ++ "Our solver\n"
       ++ show (head (improve [wikiGapped]))
       ++ "\n"
  where solvedStr g = if solved g then "Yes" else "No"

