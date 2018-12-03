module Dec2 where

import Data.Function
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

freq :: String -> Map Char Int
freq = foldl (\ m c -> Map.insertWith (+) c 1 m) Map.empty

hasN :: Int -> Map Char Int -> Bool
hasN n m = (Map.filter (==n) m) & length & (> 0)

hasTwo :: Map Char Int -> Bool
hasTwo = hasN 2

hasThree:: Map Char Int -> Bool
hasThree = hasN 3

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
  input <- readFile "./inputs/dec2.txt"
  putStrLn $ solve input
