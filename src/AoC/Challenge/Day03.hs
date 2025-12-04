module AoC.Challenge.Day03 (
  day03a,
  day03b,
)
where

import AoC.Solution
import Data.Char (digitToInt)
import Data.List (tails)

pick :: [(Int, [Int])] -> [(Int, [Int])]
pick ps =
  [ (tot * 10 + x, ys)
  | (tot, xs) <- ps
  , -- Prefer high digits!
  n <- [9, 8 .. 1]
  , x : ys <- tails xs
  , x == n
  ]

day03 :: Int -> Solution [[Int]] Int
day03 n =
  Solution
    { sParse = Right . fmap (fmap digitToInt) . lines
    , sShow = show
    , sSolve = Right . sum . fmap go
    }
    where
      go :: [Int] -> Int
      go =
        fst . (!! 0) . (!! n) . iterate pick . (: []) . (0, )

day03a :: Solution [[Int]] Int
day03a = day03 2

day03b :: Solution [[Int]] Int
day03b = day03 12
