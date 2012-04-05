module Main where

import GoogleCodeJam

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import Control.Applicative hiding (many)

main :: IO ()
main = parseAndSolve parseCases $ uncurry fileFixIt

type Path = [String]
readPath :: Parsec String u Path
readPath = char '/' >> many (noneOf "/\n") `sepBy` char '/'

parseCases :: Parsec String u [([Path],[Path])] 
parseCases = do
  numCases <- readNatural <* newline
  count numCases $ do
    numExisting <- readNatural <* char ' '
    numDesired  <- readNatural <* newline
    existing    <- count numExisting (readPath <* newline)
    desired     <- count numDesired (readPath <* newline)
    return (existing, desired)
  return []

fileFixIt :: [Path] -> [Path] -> Integer
fileFixIt exiting desired = 0
