module Main where

import GoogleCodeJam

import Control.Applicative
import Control.Arrow
import Control.Monad

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import Data.List (sort)
import qualified Data.Vector as V
import Data.Vector hiding (minimum, (++))

main :: IO ()
main = parseAndSolve parseCases (uncurry solve)

parseCases :: Parsec String u [(Integer,[Integer])] 
parseCases = do
  numCases <- readNatural <* newline
  cases <- count numCases $ do
    numCells    <- readNatural <* char ' '
    numReleases <- readNatural <* newline
    releases <- readVector numReleases <* newline
    return (numCells, releases)
  eof
  return cases

{-
 -- O(n log n), not always correct
solve :: Integer -> Integer -> [Integer] -> Integer
solve _ _ [] = 0
solve lo hi (r:rs) = hi - lo + (solve lo (x-1) lower) + (solve (x+1) hi higher)
  where d r = abs $ (hi-r) - (r-lo)
        (lower, x, higher) = foldr go ([],r,[]) rs
        go r (ls, x, hs) | d r <= d x = if x <= r then (x:ls,r,hs) else (ls,r,x:hs)
                         | otherwise  = if r <= x then (r:ls,x,hs) else (ls,x,r:hs)

-- O(n!), always correct
solve :: Integer -> Integer -> [Integer] -> Integer
solve _ _ [] = 0
solve lo hi rs = minimum $ do
  (lower, x:higher) <- map (`splitAt` rs) [0.. length rs]
  return $ (solve lo (x-1) lower) + (hi - lo) + (solve (x+1) hi higher)
-}

-- O(n^2), using caching 
solve :: Integer -> [Integer] -> Integer
solve _ [] = 0
solve numCells rs = lookup 0 (n-1)
  where rv = fromList $ 0 : sort rs ++ [numCells + 1]
        n = V.length rv
        cache = fromList  [ fromList  $ 0 : [ c + minimum cs
                                            | hi <- [ lo+2 .. n-1 ]
                                            , let c = rv!hi - rv!lo - 2
                                            , let cs =  [ lookup lo k + lookup k hi 
                                                        | k <- [lo+1..hi-1] 
                                                        ]
                                            ]
                          | lo <- [ 0 .. n - 2 ]
                          ]
        lookup lo hi = cache!lo!(hi-lo-1)
