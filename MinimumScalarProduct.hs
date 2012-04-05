module Main where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Control.Applicative

import Data.List
import GoogleCodeJam

minimumScalarProduct :: [Integer] -> [Integer] -> Integer
minimumScalarProduct as bs = sum $ zipWith (*) (sort as) (reverse $ sort bs)

parseCaseDescriptions :: Parsec String u [([Integer], [Integer])]
parseCaseDescriptions = do
  numDescriptions <- readNatural <* newline
  count numDescriptions $ do
    numDimensions <- readNatural <* newline
    as <- readVector numDimensions <* newline
    bs <- readVector numDimensions <* newline
    return (as, bs)

main = parseAndSolve parseCaseDescriptions (uncurry minimumScalarProduct)
