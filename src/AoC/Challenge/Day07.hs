{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module AoC.Challenge.Day07 (

 day07a
)
where

-- , day07b

import AoC.Solution
import Data.List (uncons)
import Data.Foldable (foldl')
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS


-- Try to be really dumb but optimal.
-- Want to see if this is much faster than the obivous
-- set stuff.
solveA :: [String] -> Int
solveA [] = error "invalid input"
solveA (l : ls) = snd $ foldl' go (IS.singleton startX, 0) ls
  where
    startX = fst . head . dropWhile ((/= 'S') . snd) $ zip [0..] l

    go :: (IntSet, Int) -> String -> (IntSet, Int)
    go (xs, count) line =
      fst $ IS.foldl' go2 ((IS.empty, count), zip [0..] line) xs
    
    go2 :: ((IntSet, Int), [(Int, Char)]) -> Int -> ((IntSet, Int), [(Int, Char)])
    go2 ((xs', c), []) _ = ((xs', c), [])
    go2 ((xs', c), line) x =
      case mht of
        Nothing -> ((xs', c), [])
        Just ((_, p), t) -> if p == '^' then ((splitBoth, c + 1), t) else ((IS.insert x xs', c), t)

      where
        mht = uncons $ dropWhile ((/= x) . fst) line
        splitUp = IS.insert (x + 1) xs'
        splitBoth = if x == 0 then splitUp else IS.insert (x - 1) splitUp


day07a :: Solution [String] Int
day07a = Solution{sParse = Right . lines , sShow = show, sSolve = Right . solveA}

day07b :: Solution _ _
day07b = Solution{sParse = Right, sShow = show, sSolve = Right}
