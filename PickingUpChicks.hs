module Main where

import Control.Applicative

import GoogleCodeJam

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import Data.List (sort)

main :: IO ()
main = parseAndSolve parseCases solve

data Case = Case  { numChicks :: Integer
                  , minRequired :: Integer
                  , barnDistance :: Integer
                  , timeLimit :: Integer
                  , positions :: [Integer]
                  , velocities :: [Integer]
                  }

newtype Solution = Solution (Maybe Integer)
instance Show Solution where
  show (Solution Nothing)   = "IMPOSSIBLE"
  show (Solution (Just n))  = show n

parseCases :: Parsec String u [Case]
parseCases = do
  numCases <- readNatural <* newline
  cases <- count numCases $ do
    numChicks     <- readNatural <* char ' '
    minRequired   <- readNatural <* char ' '
    barnDistance  <- readNatural <* char ' '
    timeLimit     <- readNatural <* newline
    positions     <- readVector (fromInteger numChicks) <* newline
    velocities    <- readVector (fromInteger numChicks) <* newline
    return $ Case { numChicks     = numChicks
                  , minRequired   = minRequired
                  , barnDistance  = barnDistance
                  , timeLimit     = timeLimit
                  , positions     = positions
                  , velocities    = velocities
                  }
  eof
  return cases

solve :: Case -> Solution
solve (Case _ minRequired barnDistance timeLimit positions velocities) = 
    Solution . check 0 0 0 . reverse . sort $ zip positions velocities
  where check swaps complete _  _ | complete == minRequired = Just swaps
        check _     _        _  []                          = Nothing
        check swaps complete incomplete ((p,v):pvs) =
          if p + v*timeLimit >= barnDistance 
            then check (swaps + incomplete) (complete + 1) incomplete pvs
            else check swaps complete (incomplete + 1) pvs
