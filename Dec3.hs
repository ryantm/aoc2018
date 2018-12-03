{-# LANGUAGE AllowAmbiguousTypes #-}

module Dec3 where

import Data.Function
import Data.List.Unique

import Data.Set (Set)
import qualified Data.Set as Set

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parsec (parse)

data Claim = Claim
             { claimId :: Int
             , x :: Int
             , y :: Int
             , w :: Int
             , h :: Int
             } deriving (Show, Eq, Ord)

pClaims :: CharParsing m => m [Claim]
pClaims = pClaim `endBy` newline

pClaim :: CharParsing m => m Claim
pClaim =
  Claim <$>
    (char '#' *> (read <$> some digit)) <*>
    (string " @ " *> (read <$> some digit)) <*>
    (char ',' *> (read <$> some digit)) <*>
    (string ": " *> (read <$> some digit)) <*>
    (char 'x' *> (read <$> some digit))

inClaim :: Int -> Int -> Claim -> Bool
inClaim px py c | px < x c = False
inClaim px py c | py < y c = False
inClaim px py c | and [px < x c + w c, py < y c + h c] = True

claimedSpots :: Claim -> Set (Int,Int)
claimedSpots c = [ (px, py) | px <- [(x c)..(x c + w c - 1)], py <- [(y c)..(y c + h c - 1)] ] &
                 Set.fromList

disjointFromAll :: [Set (Int, Int)] -> Set (Int, Int) -> Bool
disjointFromAll ss s = filter (Set.disjoint s) ss & length & (== (length ss - 1))

solve :: [Claim] -> String
solve claims =
  let sets = fmap claimedSpots claims
      intersections = [ Set.intersection s1 s2 | s1 <- sets, s2 <- sets, s1 /= s2 ]
      solve1 = intersections & fmap Set.toList & concat & sortUniq & length
      solve2 = [ claimId c | c <- claims, disjointFromAll sets (claimedSpots c)] & show
  in
   show solve1 ++ "\n" ++ solve2

main :: IO ()
main  = do
  input <- readFile "./inputs/dec3.txt"
  case parse pClaims "dec3.txt" input of
    Left pe -> print pe
    Right claims -> putStrLn $ solve claims
