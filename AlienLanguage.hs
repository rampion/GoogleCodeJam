{-# LANGUAGE TupleSections #-}
module Main where

import GoogleCodeJam

import Control.Applicative hiding ((<|>), many)

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import Data.Either

main :: IO ()
main = parseAndSolve parseCases (uncurry solveCase)

type Word = String
type Dict = [Word]
type Pattern = Parsec String () String

readLC :: Parsec String u Char
readLC = oneOf ['a'..'z']

readPattern :: Int -> Parsec String u Pattern
readPattern len = sequence <$> (count len $ literal <|> wildcard)
  where literal = char <$> readLC
        wildcard = oneOf <$> (char '(' *> many readLC <* char ')')

parseCases :: Parsec String u [(Pattern, Dict)]
parseCases = do
  wordLen <- readNatural <* char ' '
  dictLen <- readNatural <* char ' '
  numCases <- readNatural <* newline
  dict <- count dictLen $ count wordLen readLC <* newline
  patterns <- count numCases $ readPattern wordLen <* newline
  return $ map (,dict) patterns

solveCase :: Pattern -> Dict -> Int
solveCase p = length . rights . map (runParser p () "<dict>")
