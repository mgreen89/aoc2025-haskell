{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day02 (
  day02a,
  day02b,
)
where

import AoC.Solution
import Data.Bifunctor (first)
import Data.IntSet as IS
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

parser :: MP.Parsec Void String [(Int, Int)]
parser = flip
  MP.sepBy
  (MP.char ',')
  $ do
    s <- MPL.decimal
    MP.char '-'
    e <- MPL.decimal
    return (s, e)

parse :: String -> Either String [(Int, Int)]
parse =
  first MP.errorBundlePretty . MP.parse parser "day02"

rep :: Int -> Int -> Int
rep n = read . concat . replicate n . show

solve :: Bool -> [(Int, Int)] -> Int
solve b inp = sum . IS.toAscList . IS.intersection toCheck $ ns
 where
  toCheck = unions $ (\(lo, hi) -> IS.fromList [lo .. hi]) <$> inp
  maxCheck = findMax toCheck
  maxCheckLen = length . show $ maxCheck
  getNs n = IS.fromAscList . takeWhile (< maxCheck) . fmap (rep n) $ [1 ..]
  ns
    | b = IS.unions . fmap getNs $ [2 .. maxCheckLen]
    | otherwise = getNs 2

day02a :: Solution [(Int, Int)] Int
day02a = Solution{sParse = parse, sShow = show, sSolve = Right . solve False}

day02b :: Solution [(Int, Int)] Int
day02b = Solution{sParse = parse, sShow = show, sSolve = Right . solve True}
