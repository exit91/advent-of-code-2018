{-# LANGUAGE LambdaCase #-}

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import           Data.List
import           Control.Monad (forM_, when)
import           System.Environment

main :: IO ()
main = do
  getArgs >>= \case
    "1":_ -> part1
    "2":_ -> part2
    _ -> error "argument must be either 1 or 2"

input :: IO [String]
input = lines <$> readFile "02-input.txt"

------------------------------------ ------------------------------------
------------------------------------ ------------------------------------

signature :: String -> IntSet
signature = IntSet.fromList . map length . group . sort

query :: (IntSet -> Bool) -> [String] -> Int
query p = length . filter (p . signature)

part1 :: IO ()
part1 = do
  boxes <- input
  print
    $ query (\sig -> IntSet.member 2 sig) boxes
    * query (\sig -> IntSet.member 3 sig) boxes

------------------------------------ ------------------------------------
------------------------------------ ------------------------------------

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)

dist :: Eq a => [a] -> [a] -> Int
dist xs = length . filter id . zipWith (/=) xs

same :: Eq a => [a] -> [a] -> [a]
same xs = concat . zipWith (\x y -> [x | x == y]) xs

part2 :: IO ()
part2 = do
  boxes <- input
  forM_ (pairs boxes boxes) $ \(x, y) ->
    when
      (dist x y == 1)
      (print (x, y, same x y))
