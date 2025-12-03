module AoC.Challenge.Day03 (
  day03a,
  day03b,
)
where

import AoC.Solution
import Data.Char (digitToInt)
import Data.List (tails)

day03a :: Solution [[Int]] Int
day03a =
  Solution
    { sParse = Right . fmap (fmap digitToInt) . lines
    , sShow = show
    , sSolve = Right . sum . fmap go
    }
 where
  go xs =
    maximum
      [ x * 10 + y
      | x : ys <- tails xs
      , y <- ys
      ]

pick :: (Int, [Int]) -> [(Int, [Int])]
pick (tot, xs) =
  [ (tot * 10 + x, ys)
  | -- Prefer high digits!
  n <- [9, 8 .. 1]
  , x : ys <- tails xs
  , x == n
  ]

picks :: [(Int, [Int])] -> [(Int, [Int])]
picks xs =
  [ x'
  | x <- xs
  , x' <- pick x
  ]

solveB :: [[Int]] -> Int
solveB =
  sum . fmap go
 where
  go :: [Int] -> Int
  go xs =
    fst . (!! 0) . (!! 12) . iterate picks $ [(0, xs)]

day03b :: Solution [[Int]] Int
day03b =
  Solution
    { sParse = Right . fmap (fmap digitToInt) . lines
    , sShow = show
    , sSolve = Right . solveB
    }
