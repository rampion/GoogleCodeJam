{-# LANGUAGE TupleSections #-}
module Main where

import GoogleCodeJam

import Control.Applicative
import Control.Arrow
import Data.List

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import Control.Monad.Writer

main :: IO ()
main = parseAndSolve parseCases solveCase

parseCases :: Parsec String u [[(Int,Int)]]
parseCases = do
  numCases <- readNatural <* newline
  cases <- count numCases $ do 
    numRopes <- readNatural <* newline
    count numRopes $ do 
      left <- readNatural <* char ' '
      right <- readNatural <* newline
      return (left, right)
  eof
  return cases

solveCase :: [(Int,Int)] -> Int
solveCase = getSum . execWriter . mergeSort . map ((,1) . return . snd) . sort 

type IntList = ([Int], Int)

mergeSort :: [IntList] -> Writer (Sum Int) IntList
mergeSort zs@(_:_:_) = mergeSort =<< mergePass zs
mergeSort [z] = return z

mergePass :: [IntList] -> Writer (Sum Int) [IntList]
mergePass (x:y:zs) = do
  z' <- merge x y
  zs' <- mergePass zs
  return $ z' : zs'
mergePass zs = return zs

merge :: IntList -> IntList -> Writer (Sum Int) IntList
merge ([],0) y = return y
merge x ([],0) = return x
merge x@(a:as,m) y@(b:bs,n) | a <= b    = ((a:) *** (1+)) <$> merge (as,m-1) y
                            | otherwise = ((b:) *** (1+)) <$> (tell (Sum m) >> merge x (bs,n-1))
