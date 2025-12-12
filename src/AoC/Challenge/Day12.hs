module AoC.Challenge.Day12 (
  day12a,
)
where

import AoC.Solution
import Data.Bifunctor (first)
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

type Input = ([Int], [((Int, Int), [Int])])

-- Feel a bit dirty, just count the area filled in for each shape.
parser :: MP.Parsec Void String Input
parser = do
  shapes <- MP.some . MP.try $ do
    _ <- MPL.decimal
    MP.char ':'
    MP.newline
    shape <-
      MP.some $
        MP.takeWhile1P Nothing (\c -> c == '#' || c == '.') <* MP.newline
    MP.newline
    return (length . filter (== '#') . concat $ shape)
  areas <- flip MP.sepBy MP.newline $ do
    x <- MPL.decimal
    MP.char 'x'
    y <- MPL.decimal
    MP.char ':'
    MP.char ' '
    counts <- MPL.decimal `MP.sepBy` MP.char ' '
    return ((x, y), counts)
  return (shapes, areas)

parse :: String -> Either String Input
parse =
  first MP.errorBundlePretty . MP.parse parser "day12"

solveA :: Input -> Int
solveA (shapes, areas) =
  sum $ fmap go areas
 where
  go ((x, y), counts) =
    let area = x * y
        presents = sum $ zipWith (*) shapes counts
     in if (area >= presents) then 1 else 0

day12a :: Solution Input Int
day12a = Solution{sParse = parse, sShow = show, sSolve = Right . solveA}
