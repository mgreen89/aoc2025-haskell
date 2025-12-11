{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day11 (
  day11a
)
where


-- , day11b


import AoC.Solution
import AoC.Util (maybeToEither)
import Data.List (init, uncons)
import Data.Map (Map)
import qualified Data.Map as M

import Debug.Trace


-- Parse input like:
-- hhh: ccc fff iii
parse :: String -> Either String [(String, [String])]
parse =
  maybeToEither "device with no output"
  . traverse (fmap (\(x, xs) -> (init x, xs)) . uncons . words)
  . lines

solveA :: [(String, [String])] -> Either String Int
solveA devices = maybeToEither "no 'you' entry" $ M.lookup "you" pcm
  where
    pcm :: Map String Int
    pcm = fmap (sum . fmap countPaths) $ M.fromList devices

    countPaths :: String -> Int
    countPaths "out" = 1
    countPaths s = pcm M.! s

day11a :: Solution [(String, [String])] Int
day11a = Solution{sParse = parse, sShow = show, sSolve = solveA}

day11b :: Solution _ _
day11b = Solution{sParse = Right, sShow = show, sSolve = Right}
