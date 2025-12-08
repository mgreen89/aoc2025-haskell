module AoC.Challenge.Day01 (
  day01a,
  day01b,
)
where

import AoC.Solution
import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.List (scanl')
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

parser :: MP.Parsec Void String [Int]
parser = flip
  MP.sepBy
  (MP.char '\n')
  $ do
    s <- -1 <$ MP.char 'L' <|> 1 <$ MP.char 'R'
    d <- MPL.decimal
    return (s * d)

parse :: String -> Either String [Int]
parse =
  first MP.errorBundlePretty . MP.parse parser "day01"

day01a :: Solution [Int] Int
day01a =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve =
        Right
          . length
          . filter (== 0)
          . scanl' (\x y -> (x + y) `mod` 100) 50
    }

solveB :: [Int] -> Int
solveB = fst . foldl' go (0, 50)
 where
  go :: (Int, Int) -> Int -> (Int, Int)
  go (tot, curr) move = (tot', curr')
   where
    (d, curr') = (curr + move) `divMod` 100
    hits
      | move > 0 = d
      | curr' == 0 && curr == 0 = abs d
      | curr' == 0 = abs d + 1
      | curr == 0 = abs d - 1
      | otherwise = abs d
    tot' = tot + hits

day01b :: Solution [Int] Int
day01b = Solution{sParse = parse, sShow = show, sSolve = Right . solveB}
