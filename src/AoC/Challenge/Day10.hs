{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day10 (
  day10a,
)
where

-- , day10b

import AoC.Solution
import Data.Bifunctor (first)
import Data.Bits (shiftL, xor, (.|.))
import Data.Foldable (find, foldl')
import Data.List (tails)
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

-- Parse input like:
-- [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
--
-- Both the lights and buttons are stored as bitfields, where the left-most
-- light is the lowest bit.
parser :: MP.Parsec Void String [(Int, [Int])]
parser = flip MP.sepBy (MP.char '\n') $ do
  MP.char '['
  lc <- MP.takeWhile1P Nothing (\c -> c == '.' || c == '#')
  MP.char ']'
  MP.char ' '
  buttons <- MP.some $ do
    MP.char '('
    ns <- MP.sepBy MPL.decimal (MP.char ',')
    MP.char ')'
    MP.char ' '
    pure ns
  MP.char '{'
  _ <- MP.sepBy MPL.decimal (MP.char ',')
  MP.char '}'
  pure $ (mkBin lc, fmap toBits buttons)
 where
  mkBin :: String -> Int
  mkBin = foldr (\c acc -> (acc `shiftL` 1) .|. if c == '#' then 1 else 0) 0

  toBits :: [Int] -> Int
  toBits = foldl' (\acc b -> acc .|. (1 `shiftL` b)) 0

parse :: String -> Either String [(Int, [Int])]
parse =
  first MP.errorBundlePretty . MP.parse parser "day10"

minPushes :: (Int, [Int]) -> Int
minPushes (tgt, buttons) = go [(0, [], buttons)]
 where
  go :: [(Int, [Int], [Int])] -> Int
  go [] = error "no solution!"
  go st =
    let nexts = [(l `xor` b, b : ps, rest) | (l, ps, bs) <- st, (b : rest) <- tails bs]
     in case find ((== tgt) . (\(l, _, _) -> l)) nexts of
          Just (_, ps, _) -> length ps
          Nothing -> go nexts

solveA :: [(Int, [Int])] -> Int
solveA = sum . fmap minPushes

day10a :: Solution [(Int, [Int])] Int
day10a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA}

day10b :: Solution _ _
day10b = Solution{sParse = Right, sShow = show, sSolve = Right}
