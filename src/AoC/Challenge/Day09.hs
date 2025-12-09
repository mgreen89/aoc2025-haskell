module AoC.Challenge.Day09 (
  day09a,
  day09b,
)
where

import AoC.Solution
import AoC.Util (maybeToEither)
import Data.Bifunctor (first)
import Data.List (sortOn, tails)
import Data.Ord (Down (..))
import Data.Void (Void)
import Linear (V2 (..))
import Safe (headMay)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

parser :: MP.Parsec Void String [V2 Int]
parser = flip MP.sepBy (MP.char '\n') $ do
  x <- MPL.decimal <* MP.char ','
  y <- MPL.decimal
  pure $ V2 x y

parse :: String -> Either String [V2 Int]
parse =
  first MP.errorBundlePretty . MP.parse parser "day08"

sqA :: V2 Int -> V2 Int -> Int
sqA (V2 x1 y1) (V2 x2 y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

allPairs :: [a] -> [(a, a)]
allPairs xs = [(a, b) | a : as <- tails xs, b <- as]

consecPairs :: [a] -> [(a, a)]
consecPairs xs = zip xs (tail xs) ++ [(last xs, head xs)]

day09a :: Solution [V2 Int] Int
day09a =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve =
        maybeToEither "no areas"
          . headMay
          . sortOn Down
          . fmap (uncurry sqA)
          . allPairs
    }

-- Check if an axis-aligned segment intersects with an axis-aligned rectangle.
--  N.B. touching the rectangle boundary doesn't count.
-- First: rectangle
-- Second: segment
boxIntersect :: (V2 Int, V2 Int) -> (V2 Int, V2 Int) -> Bool
boxIntersect (V2 a b, V2 c d) (V2 p q, V2 r s) =
  let
    onLeft = p <= min a c && r <= min a c
    onRight = p >= max a c && r >= max a c
    above = q >= max b d && s >= max b d
    below = q <= min b d && s <= min b d
   in
    not (onLeft || onRight || above || below)

solveB :: [V2 Int] -> Either String Int
solveB pts =
  maybeToEither "no areas"
    . fmap snd
    . headMay
    . filter (noneIntersecting . fst)
    . sortOn (Down . snd)
    . fmap (\p -> (p, uncurry sqA p))
    . allPairs
    $ pts
 where
  edges :: [(V2 Int, V2 Int)]
  edges = consecPairs pts

  noneIntersecting :: (V2 Int, V2 Int) -> Bool
  noneIntersecting box =
    not . any (boxIntersect box) $ edges

day09b :: Solution [V2 Int] Int
day09b =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = solveB
    }
