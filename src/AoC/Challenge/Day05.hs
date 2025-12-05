module AoC.Challenge.Day05 (
  day05a,
  day05b,
)
where

import AoC.Solution
import Data.Bifunctor (first)
import Data.ExtendedReal (Extended (..))
import qualified Data.Interval as IV
import qualified Data.IntervalSet as IVS
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

type Input = ([(Int, Int)], [Int])

parser :: MP.Parsec Void String Input
parser = do
  fresh <- MP.some $ do
    lo <- MPL.decimal
    MP.char '-'
    hi <- MPL.decimal
    MP.char '\n'
    return (lo, hi)
  MP.char '\n'
  available <- MPL.decimal `MP.sepBy` (MP.char '\n')
  return (fresh, available)

parse :: String -> Either String Input
parse =
  first MP.errorBundlePretty . MP.parse parser "day05"

getIVS :: [(Int, Int)] -> IVS.IntervalSet Int
getIVS =
  IVS.unions
    . fmap (\(lo, hi) -> IVS.singleton ((Finite lo) IV.<=..<= (Finite hi)))

solve :: Input -> Int
solve (fresh, available) =
  length $ filter (`IVS.member` (getIVS fresh)) available

day05a :: Solution Input Int
day05a = Solution{sParse = parse, sShow = show, sSolve = Right . solve}

solveB :: Input -> Int
solveB (fresh, _) =
  sum . fmap ((+ 1) . IV.width) . IVS.toAscList . getIVS $ fresh

day05b :: Solution Input Int
day05b = Solution{sParse = parse, sShow = show, sSolve = Right . solveB}
