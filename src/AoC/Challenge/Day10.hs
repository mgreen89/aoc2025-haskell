module AoC.Challenge.Day10 (
  day10a,
  day10b,
)
where

import AoC.Solution
import AoC.Util (maybeToEither)
import Data.Bifunctor (first)
import Data.Bits (bit, shiftL, testBit, xor, (.&.), (.|.))
import Data.Foldable (find)
import Data.List (tails)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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

minJust :: [Maybe Int] -> Maybe Int
minJust = foldl' go Nothing
 where
  go :: Maybe Int -> Maybe Int -> Maybe Int
  go Nothing y = y
  go x Nothing = x
  go (Just x) (Just y) = Just $ min x y

-- Power set without using Data.Set.
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x : xs) = [x : ps | ps <- powerSet xs] ++ powerSet xs

minJoltagePushes :: (Int, [Int], [Int]) -> Either String Int
minJoltagePushes (_, buttons, joltages) =
  maybeToEither ("No solution for " ++ show joltages) $ go joltages
 where
  -- Given the set of buttons, precompute what the lights would show for
  -- each possible set of 0 or 1 pushes, and provide a function to access
  -- the button pushes for a given light configuration.
  allLightPushes :: Int -> [[Int]]
  allLightPushes ls = M.findWithDefault [] ls m
   where
    m :: Map Int [[Int]]
    m =
      M.fromListWith (++)
      . fmap (\xs -> (foldl' xor 0 xs, [xs]))
      $ powerSet buttons

  -- Get the minimum number of button pushes to achieve the requested joltages.
  -- If no solution is possible, return Nothing.
  go :: [Int] -> Maybe Int
  go js
    -- If all requested joltages are zero, we're done.
    | all (== 0) js = Just 0
    | otherwise =
        let
          -- Convert a list of joltages to a light configuration
          -- (based on the parity of the joltages).
          joltagesToLights :: [Int] -> Int
          joltagesToLights = foldr (\j acc -> (acc `shiftL` 1) .|. (j .&. 1)) 0


          -- Generate the next set of candidate joltages.
          -- For each possible set of pushes that result in the target
          -- lights, generate the new required joltages after applying
          -- those pushes.
          --
          -- Only keep valid targets (i.e. no negative joltages).
          --
          -- N.B. Let "no pushes" through so that multiple consecutive
          -- `div 2`s can happen in case e.g. 4 2 4 0 4 is much easier
          -- than 2 1 2 0 2.
          cands =
            [ (ps, js')
            | ps <- allLightPushes (joltagesToLights js)
            , let js' =
                    [ j - n
                    | (i, j) <- zip [0 ..] js
                    , let n = length $ filter (`testBit` i) ps
                    ]
            , all (>= 0) js'
            ]
         in
          -- For each possibility, reduce the joltages by half if possible
          -- and recurse, and find the minimum valid cost.
          minJust
            [ (length ps +) . (fac' *) <$> go ns'
            | (ps, js') <- cands
            , let (ns', fac') =
                    if all even js'
                      then (fmap (`div` 2) js', 2)
                      else (js', 1)
            ]

day10b :: Solution [(Int, [Int], [Int])] Int
day10b =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = fmap sum . traverse minJoltagePushes
    }
