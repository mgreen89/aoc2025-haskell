module AoC.Challenge.Day04 (
  day04a,
  day04b,
)
where

import AoC.Common.Point
import AoC.Solution
import Data.List (unfoldr)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Linear (V2 (..))

parse :: String -> Set (V2 Int)
parse = M.keysSet . M.filter (== '@') . parse2dMap . lines

reachable :: Set (V2 Int) -> Set (V2 Int)
reachable rs = S.filter f rs
 where
  f :: V2 Int -> Bool
  f = (< 4) . length . filter (`S.member` rs) . allNeighbs

day04a :: Solution (Set (V2 Int)) Int
day04a =
  Solution
    { sParse = Right . parse
    , sShow = show
    , sSolve = Right . S.size . reachable
    }

solveB :: Set (V2 Int) -> Int
solveB =
  S.size . S.unions . takeWhile (not . S.null) . unfoldr (Just . go)
 where
  go s = (r, S.difference s r)
   where
    r = reachable s

day04b :: Solution (Set (V2 Int)) Int
day04b = Solution{sParse = Right . parse, sShow = show, sSolve = Right . solveB}
