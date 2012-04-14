{-# LANGUAGE TupleSections #-}
module Main where

import GoogleCodeJam

import Text.Parsec
{-
import Text.Parsec.Char
import Text.Parsec.Combinator
-}

import Data.List
import Data.Function
import Data.Array
import Control.Arrow
import Control.Applicative

main :: IO ()
main = parseAndSolve parseCases solver

parseCases :: Parsec String u [GooglerS] 
parseCases = do
  numCases <- readNatural <* newline
  count numCases ( map GooglerC <$> many1 (noneOf "\n") <* newline ) <* eof

solver :: GooglerS -> EnglishS
solver = map convert
  where convert c | lo <= c && c <= hi = googlerToEnglish ! c
                  | otherwise          = EnglishC $ fromGooglerC c
        (lo,hi) = bounds googlerToEnglish

type EnglishS = [EnglishC]
type GooglerS = [GooglerC]

newtype EnglishC = EnglishC { fromEnglishC :: Char } deriving (Eq, Ord, Ix)
newtype GooglerC = GooglerC { fromGooglerC :: Char } deriving (Eq, Ord, Ix)

instance Show EnglishC where
  show = ("e"++) . show . fromEnglishC
  showList = (++) . map fromEnglishC

instance Show GooglerC where
  show = ("g"++) . show . fromGooglerC
  showList = (++) . map fromGooglerC

englishS :: String -> EnglishS
englishS = map EnglishC

googlerS :: String -> GooglerS
googlerS = map GooglerC

examples :: [(GooglerS,EnglishS)]
examples =  [(googlerS "y qee", 
              englishS "a zoo")
            ,(googlerS "ejp mysljylc kd kxveddknmc re jsicpdrysi"
             ,englishS "our language is impossible to understand")
            ,(googlerS "rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd"
             ,englishS "there are twenty six factorial possibilities")
            ,(googlerS "de kr kd eoya kw aej tysr re ujdr lkgc jv"
             ,englishS "so it is okay if you want to just give up")
            ,(googlerS "z"
             ,englishS "q") -- the missing case (from inspection)
            ]

derive :: (Ix a, Ord a, Ord b) => (Char -> a) -> [([a],[b])] -> Array a b
derive c = array (lo,hi) . check . clean  . (concatMap $ uncurry zip)
  where clean = map nub . groupBy ((==) `on` fst) . sort . filter (\(x,y) -> lo <= x && x <= hi)
        check as | any ((/=1) . length) as  = error "non-unique mapping"
                 | length as < 26           = error "incomplete mapping"
                 | otherwise                = map head as
        lo = c 'a'
        hi = c 'z'

googlerToEnglish :: Array GooglerC EnglishC
googlerToEnglish = derive GooglerC examples

englishToGoogler :: Array EnglishC GooglerC
englishToGoogler = derive EnglishC $ map swap examples
  where swap = uncurry $ flip (,)
