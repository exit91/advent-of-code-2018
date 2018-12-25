{-# LANGUAGE LambdaCase #-}
import           Control.Arrow (second)
import           Control.Monad.State (State)
import qualified Control.Monad.State as State
import           Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import qualified Data.IntervalMap.FingerTree as IM
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           System.Environment

main :: IO ()
main = getArgs >>= \case
  "1":_ -> part1
  "2":_ -> part2
  _ -> error "argument must be either 1 or 2"

input :: IO String
input = readFile "03-input.txt"
--input = return "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"

------------------------------------ ------------------------------------
------------------------------------ ------------------------------------

data Rect = Rect
  { x_interval :: Interval Int
  , y_interval :: Interval Int
  } deriving (Ord, Eq, Show)

newtype ID = ID Int
  deriving (Show, Ord, Eq)

-- very simplistic 'parser'
type Parser a = State String a

parse :: Parser a -> String -> a
parse pp = fst . State.runState pp

pp_num :: Parser Int
pp_num = do
  xs <- State.get
  let (ds, xr) = span (`elem` ['0'..'9']) xs
  State.put xr
  return (read ds)

pp_take :: Int -> Parser String
pp_take n = do
  xs <- State.get
  let (x,xr) = splitAt n xs
  State.put xr
  return x

pp_line :: Parser (ID, Rect)
pp_line = do
  pp_take 1 -- "#"
  no <- pp_num
  pp_take 3 -- " @ "
  x0 <- pp_num
  pp_take 1 -- ","
  y0 <- pp_num
  pp_take 2 -- ": "
  w <- pp_num
  pp_take 1 -- "x"
  h <- pp_num
  return (ID no, Rect (Interval x0 (x0 + w - 1)) (Interval y0 (y0 + h - 1)))

data RectSet = RectSet
  { x_intervals :: IntervalMap Int ()
  , y_intervals :: IntervalMap Int ()
  , x_ids :: Map (Interval Int) (Set ID)
  , y_ids :: Map (Interval Int) (Set ID)
  } deriving (Show)


multi_map_from_list :: (Ord a, Eq a, Ord b, Eq b) => [(a, b)] -> Map a (Set b)
multi_map_from_list = foldr insert Map.empty
  where
    insert (x, y) m = case Map.lookup x m of
      Just s -> Map.insert x (Set.insert y s) m
      Nothing -> Map.insert x (Set.singleton y) m

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

make_rect_set :: [(ID, Rect)] -> RectSet
make_rect_set claims = RectSet
  { x_ids = multi_map_from_list . map (swap . second x_interval) $ claims
  , y_ids = multi_map_from_list . map (swap . second y_interval) $ claims
  , x_intervals = foldr (\(_, r) m -> IM.insert (x_interval r) () m) IM.empty claims
  , y_intervals = foldr (\(_, r) m -> IM.insert (y_interval r) () m) IM.empty claims
  }

multi_app :: (Eq a, Ord a, Eq b, Ord b)
          => Map a (Set b) -> Set a -> Set b
multi_app m = Set.unions . Set.map (fromMaybe Set.empty . (`Map.lookup` m))

claims_at :: RectSet -> (Int, Int) -> Set ID
claims_at rs (px, py) =
  let xis = Set.fromList . map fst . IM.search px . x_intervals $ rs
      yis = Set.fromList . map fst . IM.search py . y_intervals $ rs
  in Set.intersection
     (multi_app (x_ids rs) xis)
     (multi_app (y_ids rs) yis)

is_multi_claimed :: RectSet -> (Int, Int) -> Bool
is_multi_claimed rs = (> 1) . Set.size . claims_at rs

bound :: RectSet -> Maybe Rect
bound rs = do
  xiv <- IM.bounds (x_intervals rs)
  yiv <- IM.bounds (y_intervals rs)
  return (Rect xiv yiv)

part1 :: IO ()
part1 = do
  rs <- make_rect_set . map (parse pp_line) . lines <$> input
  let Just (Rect xiv yiv) = bound rs
  let multi_claimed = [ (x, y)
                      | x <- [IM.low xiv .. IM.high xiv]
                      , y <- [IM.low yiv .. IM.high yiv]
                      , is_multi_claimed rs (x, y)]
  mapM_ print multi_claimed
  print (length multi_claimed)

-- NOTE Too high: 133435

------------------------------------ ------------------------------------
------------------------------------ ------------------------------------

overlaps :: Rect -> Rect -> Bool
overlaps r1 r2 =
  x_interval r1 `i_overlaps` x_interval r2
  && y_interval r1 `i_overlaps` y_interval r2
  where
    -- assume non-empty intervals
    i_overlaps :: Interval Int -> Interval Int -> Bool
    i_overlaps i1 i2 = IM.low i1 <= IM.high i2
                       && IM.low i2 <= IM.high i1

part2 :: IO ()
part2 = do
  claims <- map (parse pp_line) . lines <$> input
  let rects = map snd claims
  print $ filter (\(_, r) -> all (\x -> x == r || not (x `overlaps` r)) rects) claims
