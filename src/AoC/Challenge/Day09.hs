{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day09 (
  day09a,
)
where

-- , day09b

import AoC.Solution
import AoC.Util (maybeToEither)
import Data.Bifunctor (first)
import Data.List (sortOn, tails)
import Data.Ord (Down (..))
import Data.Void (Void)
import Linear (V2 (..))
import qualified Linear as L
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Safe (headMay)

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

day09a :: Solution [V2 Int] Int
day09a =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve =
        maybeToEither "no head"
          . headMay
          . sortOn Down
          . (\ts -> [sqA p q | p : ps <- tails ts, q <- ps])
    }

day09b :: Solution _ _
day09b = Solution{sParse = Right, sShow = show, sSolve = Right}
