module AoC.Challenge.Day08 (
  day08a,
  day08b,
)
where

import AoC.Solution
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

-- Get all the pairs sorted by increasing Euclidean distance
closestPairs :: [V3 Int] -> [(V3 Int, V3 Int)]
closestPairs js =
  -- L.qd is distance squared - no nasty floats!
  sortOn (uncurry L.qd) [(p, q) | p : ps <- tails js, q <- ps]

data State = State
  { circuits :: Set (Set (V3 Int))
  , _pairs :: [(V3 Int, V3 Int)]
  , lastConnected :: Maybe (V3 Int, V3 Int)
  }

-- Connect the next closest pair of points
connect :: State -> State
connect (State s ps _) = fromJust $ do
  ((p, q), ps') <- uncons ps
  pCirc <- find (p `S.member`) s
  s' <-
    if q `S.member` pCirc
      then Just s
      else do
        qCirc <- find (q `S.member`) s
        pure . S.insert (S.union pCirc qCirc) . S.delete pCirc . S.delete qCirc $ s
  pure (State s' ps' (Just (p, q)))

solveA :: [V3 Int] -> Int
solveA js =
  product
    . take 3
    . sortOn Down
    . fmap S.size
    . S.toList
    . (.circuits)
    . (!! 1000)
    . iterate connect
    . (\cs -> State cs (closestPairs js) Nothing)
    . S.fromList
    . fmap S.singleton
    $ js

day08a :: Solution [V3 Int] Int
day08a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA}

solveB :: [V3 Int] -> Int
solveB js =
  (\(V3 x _ _, V3 x' _ _) -> x * x')
    . fromJust
    . (.lastConnected)
    . fromJust
    . find ((== 1) . S.size . (.circuits))
    . iterate connect
    . (\s -> State s (closestPairs js) Nothing)
    . S.fromList
    . fmap S.singleton
    $ js

day08b :: Solution [V3 Int] Int
day08b = Solution{sParse = parse, sShow = show, sSolve = Right . solveB}
