module Main where

import GoogleCodeJam
import Control.Applicative hiding ((<|>))

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

type Checkin = (Int,Int)
data State = S { clock :: Int, orange :: Checkin, blue :: Checkin }
data Action = Orange Int | Blue Int

solver :: [Action] -> Int
solver = clock . foldl go (S 0 (0,1) (0,1))
  where go (S c (t,p) b) (Orange p') = let t' = max c (t + abs (p' - p)) + 1 in S t' (t',p') b
        go (S c o (t,p)) (Blue p')   = let t' = max c (t + abs (p' - p)) + 1 in S t' o (t',p')

main :: IO ()
main = parseAndSolve parseCases solver

parseCases :: Parsec String u [[Action]] 
parseCases = do
  numCases <- readNatural <* newline
  cases <- count numCases $ do
    numActions <- readNatural
    actions <- count numActions $ do
      char ' '
      f <- (char 'O' *> return Orange) <|> (char 'B' *> return Blue)
      char ' '
      f <$> readNatural
    newline
    return actions
  eof
  return cases
