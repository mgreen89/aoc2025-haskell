module AoC.Challenge.Day10 (
  day10a,
  day10b,
)
where

import AoC.Solution
import Data.Bifunctor (first)
import Data.Bits (bit, shiftL, testBit, xor, (.&.), (.|.))
import Data.Foldable (find)
import Data.List (tails)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL

-- Parse input like:
-- [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
--
-- Both the lights and buttons are stored as bitfields, where the left-most
-- light is the lowest bit.
parser :: MP.Parsec Void String [(Int, [Int], [Int])]
parser = flip MP.sepBy (MP.char '\n') $ do
  MP.char '['
  lc <- MP.takeWhile1P Nothing (\c -> c == '.' || c == '#')
  MP.char ']'
  MP.char ' '
  buttons <- MP.some $ do
    MP.char '('
    ns <- MP.sepBy MPL.decimal (MP.char ',')
    MP.char ')'
    MP.char ' '
    pure ns
  MP.char '{'
  joltages <- MP.sepBy MPL.decimal (MP.char ',')
  MP.char '}'
  pure $ (mkBin lc, fmap toBits buttons, joltages)
 where
  mkBin :: String -> Int
  mkBin = foldr (\c acc -> (acc `shiftL` 1) .|. if c == '#' then 1 else 0) 0

  toBits :: [Int] -> Int
  toBits = foldl' (\acc b -> acc .|. (bit b)) 0

parse :: String -> Either String [(Int, [Int], [Int])]
parse =
  first MP.errorBundlePretty . MP.parse parser "day10"

minLightPushes :: (Int, [Int], [Int]) -> [Int]
minLightPushes (tgt, buttons, _) = go [(0, [], buttons)]
 where
  go :: [(Int, [Int], [Int])] -> [Int]
  go [] = error "no solution!"
  go st =
    let nexts =
          [ (l `xor` b, b : ps, rest)
          | (l, ps, bs) <- st
          , (b : rest) <- tails bs
          ]
     in case find ((== tgt) . (\(l, _, _) -> l)) nexts of
          Just (_, ps, _) -> ps
          Nothing -> go nexts

day10a :: Solution [(Int, [Int], [Int])] Int
day10a =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . sum . fmap (length . minLightPushes)
    }

-- Power set without using Set.
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x : xs) = [x : ps | ps <- powerSet xs] ++ powerSet xs

minJoltagePushes :: (Int, [Int], [Int]) -> Int
minJoltagePushes (_, buttons, joltages) =
  go (joltages, 1)
 where
  -- Get all ways of making the lights turn on (pushing each button
  -- at most once).
  -- Memoize the results for performance.
  allLightPushes :: Int -> [[Int]]
  allLightPushes = (memo M.!)
   where
    getPushes :: Int -> [[Int]]
    getPushes t = filter ((== t) . (foldl' xor 0)) $ powerSet buttons

    memo :: Map Int [[Int]]
    memo =
      M.fromList
        . fmap (\l -> (l, getPushes l))
        $ [0 .. (2 ^ length joltages) - 1]

  go :: ([Int], Int) -> Int
  go (js, mfac)
    -- If all requested joltages are zero, we're done.
    | all (== 0) js = 0
    | otherwise =
        let
          -- Convert the joltages to lights.
          tgt = foldr (\j acc -> (acc `shiftL` 1) .|. (j .&. 1)) 0 js

          -- Generate the new joltages after applying all possible pushes.
          -- to get the lights.
          --
          -- N.B. Let "no pushes" through so that multiple consecutive
          -- `div 2`s can happen in case e.g. 4 2 4 0 4 is much easier
          -- than 2 1 2 0 2.
          nexts =
            [ (ps, ns)
            | ps <- allLightPushes tgt
            , let ns =
                    [ j - n
                    | (i, j) <- zip [0 ..] js
                    , let n = length $ filter (\p -> p `testBit` i) ps
                    ]
            , all (>= 0) ns
            ]

          -- Factor out 2 from the joltages where possible.
          red :: ([Int], Int) -> ([Int], Int)
          red (xs, m)
            | all even xs = (fmap (`div` 2) xs, m * 2)
            | otherwise = (xs, m)

          -- Reduce the joltages as much as possible.
          nxm =
            [ mfac * length ps + go (ns', fac')
            | (ps, ns) <- nexts
            , let (ns', fac') = red (ns, mfac)
            ]
         in
          case nxm of
            -- Large enough number this path is never taken.
            [] -> 1000000000
            cs -> minimum cs

day10b :: Solution [(Int, [Int], [Int])] Int
day10b =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . sum . fmap minJoltagePushes
    }
