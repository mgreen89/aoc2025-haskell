module AoC.Challenge.Day06 (
  day06a,
  day06b,
)
where

import AoC.Solution
import Data.List (transpose)
import Data.List.Split (splitWhen)

day06a :: Solution [[String]] Int
day06a =
  Solution
    { sParse = Right . fmap words . lines
    , sShow = show
    , sSolve = Right . sum . fmap (go . reverse) . transpose
    }
 where
  go :: [String] -> Int
  go ("+" : xs) = sum (fmap read xs)
  go ("*" : xs) = product (fmap read xs)
  go x = error ("invalid operation: " ++ show x)

day06b :: Solution [String] Int
day06b =
  Solution
    { sParse = Right . lines
    , sShow = show
    , sSolve = Right . sum . fmap solveProb . splitProbs
    }
 where
  splitProbs :: [String] -> [[String]]
  splitProbs = splitWhen (all (== ' ')) . transpose

  getOp :: Char -> ([Int] -> Int)
  getOp '+' = sum
  getOp '*' = product
  getOp x = error ("invalid operation: " ++ show x)

  solveProb :: [String] -> Int
  solveProb xs =
    -- Each String is either a set of numbers (with preceeding and/or
    -- following spaces), or a set of numbers with a following
    -- operation. The operation always comes on the first number.
    op (fmap (read . init) xs)
   where
    op = getOp . last . head $ xs
