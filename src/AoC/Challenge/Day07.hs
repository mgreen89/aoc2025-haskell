module AoC.Challenge.Day07 (
  day07a,
  day07b,
)
where

import AoC.Solution
import qualified Data.IntMap as IM
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (uncons)

-- Try to be really dumb but optimal.
-- Want to see if this is much faster than the obivous
-- set stuff.
solveA :: String -> Int
solveA inp = case lines inp of
  [] -> error "invalid input"
  (l : ls) -> snd $ foldl' go (IS.singleton startX, 0) ls
   where
    startX = fst . head . dropWhile ((/= 'S') . snd) $ zip [0 ..] l
 where
  go :: (IntSet, Int) -> String -> (IntSet, Int)
  go (xs, count) line =
    fst $ IS.foldl' go2 ((IS.empty, count), zip [0 ..] line) xs

  go2 :: ((IntSet, Int), [(Int, Char)]) -> Int -> ((IntSet, Int), [(Int, Char)])
  go2 ((xs', c), []) _ = ((xs', c), [])
  go2 ((xs', c), line) x =
    case mht of
      Nothing -> ((xs', c), [])
      Just ((_, p), t) ->
        if p == '^'
          then ((splitBoth, c + 1), t)
          else ((IS.insert x xs', c), t)
   where
    mht = uncons $ dropWhile ((/= x) . fst) line
    splitUp = IS.insert (x + 1) xs'
    splitBoth = if x == 0 then splitUp else IS.insert (x - 1) splitUp

day07a :: Solution String Int
day07a = Solution{sParse = Right, sShow = show, sSolve = Right . solveA}

solveB :: String -> Int
solveB inp = case lines inp of
  [] -> error "invalid input"
  (l : ls) -> IM.foldl' (+) 0 . fst $ foldl' go (IM.singleton startX 1, 0) ls
   where
    startX = fst . head . dropWhile ((/= 'S') . snd) $ zip [0 ..] l
 where
  go :: (IntMap Int, Int) -> String -> (IntMap Int, Int)
  go (xs, count) line =
    fst $ IM.foldlWithKey' go2 ((IM.empty, count), zip [0 ..] line) xs

  go2 ::
    ((IntMap Int, Int), [(Int, Char)]) ->
    Int ->
    Int ->
    ((IntMap Int, Int), [(Int, Char)])
  go2 ((xs', c), []) _ _ = ((xs', c), [])
  go2 ((xs', c), line) x rs =
    case mht of
      Nothing -> ((xs', c), [])
      Just ((_, p), t) ->
        if p == '^'
          then ((splitBoth, c + 1), t)
          else ((IM.insertWith (+) x rs xs', c), t)
   where
    mht = uncons $ dropWhile ((/= x) . fst) line
    splitUp = IM.insertWith (+) (x + 1) rs xs'
    splitBoth = if x == 0 then splitUp else IM.insertWith (+) (x - 1) rs splitUp

day07b :: Solution String Int
day07b = Solution{sParse = Right, sShow = show, sSolve = Right . solveB}
