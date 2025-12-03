{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day03 (
  day03a,
)
where

-- , day03b

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

day03b :: Solution _ _
day03b = Solution{sParse = Right, sShow = show, sSolve = Right}
