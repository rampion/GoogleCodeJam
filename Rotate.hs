{-# LANGUAGE TupleSections, TypeSynonymInstances #-}
module Main where

import GoogleCodeJam

import Control.Applicative
import Control.Arrow
import Data.Array
import Data.List (intercalate, transpose)

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

main :: IO ()
main = parseAndSolve parseCases $ uncurry solveCase

data Piece = RedPiece | BluePiece | EmptySlot deriving Eq
type Board = Array (Int,Int) Piece
newtype ShowBoard = ShowBoard Board

instance Show Piece where
  show RedPiece = "R"
  show BluePiece = "B"
  show EmptySlot = "."

instance Show ShowBoard where
  show (ShowBoard arr) = concat [ "\n" ++ concatMap show [ arr ! (i,j) | i <- [1..n] ] | j <- [1..n] ]
    where b@((1,1),(n,_)) = bounds arr

parseCases :: Parsec String u [(Board, Int)]
parseCases = do 
  numCases <- readNatural <* newline
  cases <- count numCases $ do 
    side <- readNatural <* char ' '
    win  <- readNatural <* newline
    let lookup '.' = EmptySlot
        lookup 'R' = RedPiece
        lookup 'B' = BluePiece
    rows <- count side $ count side (lookup <$> oneOf ".RB") <* newline
    return (listArray ((1,1),(side,side)) . concat $ transpose rows, win)
  eof
  return cases

data Winner = Neither | Red | Blue | Both deriving Show

solveCase :: Board -> Int -> Winner
solveCase b k = search k  . fall . rotate $ b

rotate :: Board -> Board
rotate arr = ixmap b (\(i,j) -> (j,n+1-i)) arr
  where b@((1,1),(n,_)) = bounds arr

fall :: Board -> Board
fall arr = array b $ [ pt
                     | i <- [1..n]
                     , let drop j d = ( (i, d+j), arr ! (i,j) )
                     , let dists = scanl (+) 0 $ [ fromEnum $ arr!(i,j) == EmptySlot | j <- column ]
                     , pt <- map ((,EmptySlot) . (i,)) [1..n] ++ zipWith drop column dists
                     ]
  where b@((1,1),(n,_)) = bounds arr
        column = [n,n-1..1]

search :: Int -> Board -> Winner
search k arr = case (winFor RedPiece, winFor BluePiece) of
                  (True, True) -> Both
                  (True, _)    -> Red
                  (_, True)    -> Blue
                  (_,_)        -> Neither
  where b@((1,1),(n,_)) = bounds arr
        -- check backwards, so we can skip forward fast
        check p step k' pt  | k' == 0             = True
                            | not (inRange b pt)  = False 
                            | arr!pt == p         = check p step (k' - 1) ((-1) `step` pt)
                            | otherwise           = check p step k (k `step` pt)
        winFor p = any id $ 
          -- start at the left edge, checking for --
          [ check p (first . (+)) k (k,j)  | j <- [1..n] ] ++
          -- start at the top edge, checking for |
          [ check p (second . (+)) k (i,k) | i <- [1..n] ] ++
          -- start at the left and top edges, checking for \
          [ check p (\s -> (s+) *** (s+)) k pt   | pt <- map (k,) [k..n] ++ map (,k) [k+1..n] ] ++
          -- start at the left and bottom edges, checking for /
          [ check p (\s -> (s+) *** (-s+)) k pt  | pt <- map (k,) [1..n-k+1] ++ map (,n-k+1) [k+1..n] ]
