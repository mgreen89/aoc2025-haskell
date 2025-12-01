{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day01 (
  day01a,
)
where

-- , day01b

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
    , sSolve = Right . length . filter (== 0) . scanl' (\x y -> (x + y) `mod` 100) 50
    }

day01b :: Solution _ _
day01b = Solution{sParse = Right, sShow = show, sSolve = Right}
