module Dec2 where

import Data.Function
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

freq :: String -> Map Char Int
freq [] = Map.empty
freq (c:cs) = Map.insertWith (+) c 1 (freq cs)

hasTwo :: Map Char Int -> Bool
hasTwo m = (Map.filter (==2) m) & length & (> 0)

hasThree:: Map Char Int -> Bool
hasThree m = (Map.filter (==3) m) & length & (> 0)

diffByExactlyOne :: String -> String -> Bool
diffByExactlyOne one two =
  zip one two & filter (\(a,b) -> a /= b) & length & (==1)

common :: String -> String -> String
common one two =
  zip one two & filter (\(a,b) -> a == b) & unzip & fst

solve :: String -> String
solve input =
  let preprocess = input & lines
      freqs = map freq preprocess
      solve1 = (filter hasTwo freqs & length) *  (filter hasThree freqs & length) & show
      solve2 = [ common x y | x <- preprocess, y <- preprocess, diffByExactlyOne x y ] & head
  in
   solve1 ++ "\n" ++ solve2

main :: IO ()
main  = do
  interact solve