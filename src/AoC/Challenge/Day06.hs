module AoC.Challenge.Day06 (
  day06a,
  day06b,
)
where

import AoC.Solution
import AoC.Util (maybeToEither)
import Data.List (transpose, uncons)
import Data.List.Split (splitWhen)
import Text.Read (readEither)

getOp :: (Foldable f, Num a) => Char -> Either String (f a -> a)
getOp '+' = Right sum
getOp '*' = Right product
getOp x = Left ("Invalid operation: " ++ show x)

-- Not available in this version of base
unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = (\(hd, tl) -> (reverse tl, hd)) <$> uncons (reverse xs)

day06a :: Solution [[String]] Int
day06a =
  Solution
    { sParse = Right . fmap words . lines
    , sShow = show
    , sSolve = fmap sum . traverse (go . reverse) . transpose
    }
 where
  go :: [String] -> Either String Int
  go ([c] : xs) = getOp c <*> traverse readEither xs
  go _ = Left "Invalid strucutre"

day06b :: Solution [String] Int
day06b =
  Solution
    { sParse = Right . lines
    , sShow = show
    , sSolve = fmap sum . traverse solveProb . splitProbs
    }
 where
  splitProbs :: [String] -> [[String]]
  splitProbs = splitWhen (all (== ' ')) . transpose

  -- Each String is either a set of numbers (with preceeding and/or
  -- following spaces), or a set of numbers with a following
  -- operation. The operation always comes on the first number.
  solveProb :: [String] -> Either String Int
  solveProb [] = Left "No problem to solve"
  solveProb (x : xs) = do
    (fstN, opC) <- maybeToEither "Empty problem columnn" (unsnoc x)
    op <- getOp opC
    fstNum <- readEither fstN
    rest <- traverse readEither xs
    return $ op (fstNum : rest)
