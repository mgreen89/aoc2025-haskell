{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day04 (
  day04a,
)
where

-- , day04b

import AoC.Common.Point
import AoC.Solution
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))

solve :: Map (V2 Int) Char -> Int
solve m =
  S.size $ S.filter f rolls
 where
  rolls :: Set (V2 Int)
  rolls = M.keysSet . M.filter (== '@') $ m

  f :: V2 Int -> Bool
  f = (< 4) . length . filter (`S.member` rolls) . allNeighbs

day04a :: Solution (Map (V2 Int) Char) Int
day04a = Solution{sParse = Right . parse2dMap . lines, sShow = show, sSolve = Right . solve}

day04b :: Solution _ _
day04b = Solution{sParse = Right, sShow = show, sSolve = Right}
