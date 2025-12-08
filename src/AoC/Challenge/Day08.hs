{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day08 (
  day08a,
)
where

-- , day08b

import AoC.Solution
import Control.Monad (replicateM)
import Data.Bifunctor (first)
import Data.Foldable (find)
import Data.List (sortOn, tails, uncons)
import Data.Maybe (fromJust)
import Data.Ord (Down (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void (Void)
import Linear (V3 (..))
import qualified Linear as L
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

parser :: MP.Parsec Void String [V3 Int]
parser = flip MP.sepBy (MP.char '\n') $ do
  x <- MPL.decimal <* MP.char ','
  y <- MPL.decimal <* MP.char ','
  z <- MPL.decimal
  pure $ V3 x y z

parse :: String -> Either String [V3 Int]
parse =
  first MP.errorBundlePretty . MP.parse parser "day08"

solveA :: [V3 Int] -> Int
solveA js =
  product
    . take 3
    . sortOn Down
    . fmap S.size
    . S.toList
    . fst
    . (!! 1000)
    . iterate go
    . (,pairs)
    . S.fromList
    . fmap S.singleton
    $ js
 where
  -- L.qd is distance squared - no nasty floats!
  pairs = sortOn (uncurry L.qd) [(p, q) | p : ps <- tails js, q <- ps]

  go :: (Set (Set (V3 Int)), [(V3 Int, V3 Int)]) -> (Set (Set (V3 Int)), [(V3 Int, V3 Int)])
  go (s, ps) = fromJust $ do
    ((p, q), ps') <- uncons ps
    pCirc <- find (p `S.member`) s
    s' <-
      if q `S.member` pCirc
        then Just s
        else do
          qCirc <- find (q `S.member`) s
          pure . S.insert (S.union pCirc qCirc) . S.delete pCirc . S.delete qCirc $ s
    pure (s', ps')

day08a :: Solution [V3 Int] Int
day08a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA}

day08b :: Solution _ _
day08b = Solution{sParse = Right, sShow = show, sSolve = Right}
