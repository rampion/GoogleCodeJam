module Main where

import GoogleCodeJam

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

import Control.Applicative hiding (many, empty)
import Control.Monad.Writer
import Data.Map
import Prelude hiding (lookup)

main :: IO ()
main = parseAndSolve parseCases $ uncurry fileFixIt

type Dir = String
type Path = [Dir]
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

newtype PathTrie = PathTrie (Map Dir PathTrie)

wrap :: Monad m => Map Dir PathTrie -> m PathTrie
wrap = return . PathTrie

build :: Path -> PathTrie -> Writer (Sum Integer) PathTrie
build []     pt           = return pt
build (d:ds) (PathTrie t) = wrap . (insert d `flip` t) 
                            =<< build ds 
                            =<< maybe (tell (Sum 1) >> wrap empty) return (lookup d t)

buildAll :: [Path] -> PathTrie -> Writer (Sum Integer) PathTrie
buildAll = flip . foldM $ flip build
  
fileFixIt :: [Path] -> [Path] -> Integer
fileFixIt existing desired = getSum . execWriter $ 
  buildAll desired =<< const mempty `censor`buildAll existing (PathTrie empty)
