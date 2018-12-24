import Data.Set (Set)
import qualified Data.Set as Set
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "1":_ -> part1
    "2":_ -> part2
    _ -> error "argument must be 1 or 2"

------------------------------------ ------------------------------------
------------------------------------ ------------------------------------

input :: IO String
--input = getContents
input = readFile "01-input.txt"

readInt :: String -> Int
readInt xs = case xs of
  '+':xr -> read xr
  _ -> read xs

changes :: String -> [Int]
changes = map readInt . lines

accum :: Num a => [a] -> [a]
accum = scanl (+) 0

part1 :: IO ()
part1 = input >>= print . last . accum . changes

------------------------------------ ------------------------------------
------------------------------------ ------------------------------------

-- fold with abort
foldlA :: (a -> b -> Either c a) -> a -> [b] -> Either c a
foldlA _ a [] = Right a
foldlA f a (b:br) = case f a b of
  Right a' -> foldlA f a' br
  Left c -> Left c


twice :: (Eq a, Ord a) => [a] -> Either a (Set a)
twice = foldlA f Set.empty
  where
    f visited new
      | new `Set.member` visited = Left new
      | otherwise          = Right (Set.insert new visited)

part2 :: IO ()
part2 = input >>= print . twice . accum . cycle . changes
