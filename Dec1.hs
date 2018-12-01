module Dec1 where

import Data.Function
import qualified Data.Set as Set
import Data.Set (Set)


removePlus :: String -> String
removePlus ('+':xs) = xs
removePlus s = s

findRepeat :: Set Int -> Int -> [Int] -> Int
findRepeat _ _ [] = error "no repeat found"
findRepeat past acc (i:is)
  | Set.member (acc + i) past = acc + i
  | otherwise = let new = acc + i
                in
                 findRepeat (Set.insert new past) new is

solve :: String -> String
solve input =
  let preprocess = input & lines & fmap removePlus & fmap read
      solve1 = preprocess & sum & show
      solve2 = preprocess & cycle & findRepeat (Set.fromList [0]) 0 & show
  in
   solve1 ++ "\n" ++ solve2

main :: IO ()
main  = do
  interact solve
