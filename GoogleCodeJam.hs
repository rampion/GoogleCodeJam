{-# LANGUAGE RankNTypes #-}
module GoogleCodeJam where

import Control.Applicative
import Control.Arrow
import Control.Monad

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import System.IO

import ReadArgs

readNatural :: (Read a, Num a) => Parsec String u a
readNatural = read <$> many1 digit

readInteger :: (Read a, Num a) => Parsec String u a
readInteger = option id (const negate <$> char '-') <*> readNatural

readVector :: (Read a, Num a) => Int -> Parsec String u [a]
readVector 0 = return []
readVector n = (:) <$> (readInteger <* terminator) <*> readVector (n-1)
  where terminator = if n == 1 then return ' ' else space

parseAndSolve :: Show b => (forall u. Parsec String u [a]) -> (a -> b) -> IO ()
parseAndSolve parser solver = do
  (input :& output) <- readArgs
  -- use stdin if the input isn't given
  let (getProblem, name) = maybe (getContents, "<stdin>") (readFile &&& id) input
  problem <- getProblem
  case runParser parser () name problem of
    Left err    -> hPutStrLn stderr $ "Error: " ++ show err
    Right cases -> 
      -- use stdout if the output isn't given
      maybe ($ stdout) (`withFile` WriteMode) output $ \h -> zipWithM_ (\i v -> 
          hPutStrLn h $ "Case #" ++ show i ++ ": " ++ show v
        ) [1..] (map solver cases)
