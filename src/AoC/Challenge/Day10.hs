module AoC.Challenge.Day10 (
  day10a,
  day10b,
)
where

import AoC.Solution
import Data.Bifunctor (first)
import Data.Bits (bit, shiftL, testBit, xor, (.|.))
import Data.Foldable (find)
import Data.List (tails)
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

minLightPushes :: (Int, [Int], [Int]) -> Int
minLightPushes (tgt, buttons, _) = go [(0, [], buttons)]
 where
  go :: [(Int, [Int], [Int])] -> Int
  go [] = error "no solution!"
  go st =
    let nexts =
          [ (l `xor` b, b : ps, rest)
          | (l, ps, bs) <- st
          , (b : rest) <- tails bs
          ]
     in case find ((== tgt) . (\(l, _, _) -> l)) nexts of
          Just (_, ps, _) -> length ps
          Nothing -> go nexts

day10a :: Solution [(Int, [Int], [Int])] Int
day10a =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . sum . fmap minLightPushes
    }

minJoltagePushes :: (Int, [Int], [Int]) -> Int
minJoltagePushes (_, buttons, joltages) =
  go [(replicate (length joltages) 0, [], buttons)]
 where
  go :: [([Int], [Int], [Int])] -> Int
  go st =
    let nexts =
          [ (js', b : ps, (b : cands))
          | (js, ps, bs) <- st
          , (b : cands) <- tails bs
          , let js' = update js b
          , all (\(j, t) -> j <= t) $ zip js' joltages
          ]
     in case find ((== joltages) . (\(js, _, _) -> js)) nexts of
          Just (_, ps, _) -> length ps
          Nothing -> go nexts

  update :: [Int] -> Int -> [Int]
  update js b =
    fmap (\(i, j) -> if b `testBit` i then j + 1 else j) . zip [0 ..] $ js

day10b :: Solution [(Int, [Int], [Int])] Int
day10b =
  Solution
    { sParse = parse
    , sShow = show
    , sSolve = Right . sum . fmap minJoltagePushes
    }
