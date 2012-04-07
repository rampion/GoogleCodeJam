module Main where

import GoogleCodeJam

import Control.Applicative
import Control.Arrow
import Control.Monad

import Data.Set as S
import Data.Map as M

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

main :: IO ()
main = parseAndSolve parseCases solve

parseCases :: Parsec String u [String] 
parseCases = do
  numCases <- readNatural <* newline
  count numCases (many1 (noneOf "\n") <* newline) <* eof

solve :: String -> Integer
solve s = foldl (\n c -> n*base + encoding!c) 0 s
  where base = max 2 (toInteger . S.size $ foldr S.insert S.empty s)
        lup 0 = 1
        lup 1 = 0
        lup n = toInteger n
        encoding = foldl (\m c -> insertWith (flip const) c (lup $ M.size m) m) M.empty s
  
