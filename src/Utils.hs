{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Utils (
  -- Parsing in the file
  getLines
  , getF
  , getWords
  , getT -- gets .ex file
  , getNumbers
  , numbers
  , numbers2
  , numbers3
  , numbersSigned

  -- text manipulation
  , wordsWhen
  , splitOn
  , chunksOf
  , splitWhen
  , wordsBy


  -- ReadP parsing functions
  , parseS
  , many
  , many1
  , sepBy
  , sepBy1
  , endBy
  , endBy1
  , satisfy
  , pInt
  , digit
  , isDigit

  -- Coordinates
  , Coord
  , Coord3
  , Coord4
  , Direction(..)
  , toCoord
  , scale2
  , scale3
  , scale4
  , neighbours4
  , neighbourCoords4
  , neighbours6
  , neighbours8
  , nextTo8
  , euclidian
  , manhattan
  , circle
  , manhattan3
  , lt, rt, up, dn
  , lt3, rt3, up3, dn3, in3, ot3
  , leftOf, rightOf, above, below
  , directions4
  , directions8
  , clockTurn
  , antiTurn

  -- from Data.Maybe
  , fromJust
  , fromMaybe
  , isJust
  , isNothing
  , catMaybes
  , mapMaybe
  
  -- from Data.List
  , sort
  , group
  , groupBy
  , sortBy 
  , minimumBy
  , maximumBy
  , sortOn
  , elemIndex 
  , findIndex 
  , nub
  , intercalate
  , transpose
  , partition
  , unfoldr
  , foldl'
  , (\\)

  -- from Data.Char
  , ord
  , chr

  -- from Data.Bifunctor
  , bimap 
  , first
  , second
  
  -- from Data.Function
  , on

  -- from Data.Either
  , lefts
  , rights
  
  -- from TimeIt
  , timeIt
  
  -- from Debug.Trace
  , trace

  -- some of my functions
  , binSearch
  , floodFill
  , bfs
  , dijkstra
  , dfs
  , dfsSet
  , fromRight
  , levi
  , crossProduct
  , parseGridWith
  , steadyState
  , steadyStateM
  , times

  -- from Data.Ord
  , comparing
  , Down(Down)

  -- from Data.Foldable
  , traverse_

  -- from Text.ParserCombinators.ReadP
  , ReadP
  , manyTill
  , lookAhead
  , string
  , char
  , isAscii
  , anyChar

  , swap

  -- my recursion functions
  , Algebra
  , Coalgebra
  , ana
  , cata
  , hylo
  , TreeF(..)
  , ListF(..)
  , TreeF'(..)
  , Tree
  , ForestF(..)
  , Fix(..)
) where


import Data.Char ( ord, isDigit, chr, isAscii )
import Data.Tuple (swap)
import Data.List.Split (chunksOf, wordsBy)
import Data.Maybe ( fromJust, fromMaybe, isJust, isNothing, catMaybes, mapMaybe )
import Data.List -- ( elemIndex, findIndex, group, groupBy, sort, sortBy, sortOn, nub, intercalate, transpose, minimumBy, maximumBy ) 
import Data.Bifunctor ( Bifunctor(second, bimap, first) )
import Data.Function ( on )
import Data.Either ( lefts, rights, fromRight )
import System.TimeIt ( timeIt )
import Text.ParserCombinators.ReadP (ReadP, many1, readP_to_S, satisfy, string, many, sepBy, sepBy1, endBy, endBy1, char, manyTill, look)
import Text.Parser.LookAhead
import Data.Hashable ( Hashable )
import Data.Foldable ( traverse_ )
import Debug.Trace (trace)
import qualified Data.Set as S
import Data.Ord ( comparing, Down(Down) )
import Data.MemoTrie ( HasTrie(..) )
import qualified Queue as Q
import qualified Data.PQueue.Min as QQ


--- Things to add

-- Rectangular grid with focus and distributive, representable instances
-- Directions, rotations...

------------ GET THE INPUT FROM FILE ------------------

-- Gets the .in file from AOC
getF :: (String -> a) -> Int -> IO a
getF f n = do
  s <- readFile $ "./data/Part" ++ show n ++ ".in"
  return $ f s



-- Gets an alternative test file with .ex ending
getT :: (String -> a) -> Int -> IO a
getT f n = do
  s <- readFile $ "./data/Part" ++ show n ++ ".ex"
  return $ f s


getRaw :: Int -> IO String
getRaw = getF id


getWords :: Int -> IO [String]
getWords = getF words


getLines :: Int -> IO [String]
getLines = getF lines


getNumbers :: Int -> IO [Int]
getNumbers = getF numbers


--, Y+17
numbers :: String -> [Int]
numbers [] = []
numbers cs
  | null f = numbers $ dropWhile (not . isDigit) cs
  | otherwise = read f : numbers b
  where
    (f,b) = span isDigit cs

numbersSigned :: String -> [Int]
numbersSigned [] = []
numbersSigned cs
  | null f = numbersSigned $ dropWhile (\c -> not (isDigit c|| (c=='-'))) cs
  | otherwise = read f : numbersSigned b
  where
    (f,b) = span (\c -> isDigit c || c == '-') cs


numbers2 :: [Char] -> (Int, Int)
numbers2 s = (ns!!0, ns!!1)
  where
    ns = numbers s


numbers3 :: [Char] -> (Int, Int, Int)
numbers3 s = (ns!!0, ns!!1, ns!!2)
  where
    ns = numbers s

-- splits a list on an item and deletes the item

{-# INLINABLE splitOn #-}
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn s = go [] []
  where
    go acc next [] = acc ++ [next]
    go acc next (l:ls)
      | l == s = go (acc ++ [next]) [] ls
      | otherwise = go acc (next ++ [l]) ls


wordsWhen     :: (a -> Bool) -> [a] -> [[a]]
wordsWhen p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p = go [] []
  where
    go acc next [] = acc ++ [next]
    go acc next (l:ls)
      | p l = go (acc ++ [next]) [] ls
      | otherwise = go acc (next ++ [l]) ls


parseS :: ReadP a -> ReadS a
parseS = readP_to_S


digit :: ReadP Char
digit = satisfy (`elem` "0123456789")

pInt :: ReadP Int
pInt = do
  n <- many1 digit
  return $ read n


anyChar :: ReadP Char
anyChar = satisfy isAscii

------------------ VARIOUS UTILITY FUNCTIONS --------------------


-- Strict...
times :: Int -> (a -> a) -> a -> a
times n f x
  | n <= 0    = x
  | otherwise = times (n-1) f $! f x


binSearch :: Integral t => (t -> Bool) -> t -> t -> t
binSearch p = go
  where
    go lo hi
      | lo + 1 == hi = hi
      | p mid = go lo mid
      | otherwise = go mid hi
      where
        mid = (lo + hi) `div` 2


steadyState :: Eq a => (a -> a) -> a -> a
steadyState f x = if f x == x then x else steadyState f (f x)


steadyStateM :: Eq a => (a -> Maybe a) -> a -> Maybe a
steadyStateM f x = case f x of
                     Nothing -> Just x
                     Just y -> steadyStateM f y



------------------------ COORDINATE / VECTOR STUFF ------------


type Coord = (Int, Int)
type Coord3 = (Int, Int, Int)
data Direction = RT | LF | UP | DN deriving (Eq, Show)

toCoord :: Direction -> Coord
toCoord RT = rt
toCoord LF = lt
toCoord UP = up
toCoord DN = dn

instance {-# OVERLAPPING #-} Hashable Coord where


instance Num Coord where
  {-# INLINE (+) #-}
  (+), (-), (*) :: Coord -> Coord -> Coord
  (x1, y1) + (x2, y2) = (x1+x2, y1+y2)
  (x1, y1) - (x2, y2) = (x1-x2, y1-y2)
  (x1, y1) * (x2, y2) = (x1*x2, y1*y2)
  abs, signum :: Coord -> Coord
  abs (x, y) = (abs x, abs y)
  signum (x, y) = (signum x, signum y)
  fromInteger :: Integer -> Coord
  fromInteger i = (fromInteger i, 0)


instance Num Coord3 where
  (x1, y1, z1) + (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)
  (x1, y1, z1) - (x2, y2, z2) = (x1-x2, y1-y2, z1-z2)
  (x1, y1, z1) * (x2, y2, z2) = (x1*x2, y1*y2, z1*z2)
  abs (x, y, z) = (abs x, abs y, abs z)
  signum (x, y, z) = (signum x, signum y, signum z)
  fromInteger i = (fromInteger i, 0, 0)


scale4 :: Int -> Coord4 -> Coord4
scale4 k (x,y,z,a) = (k*x, k*y, k*z, k*a)

scale3 :: Int -> Coord3 -> Coord3
scale3 k (x,y,z) = (k*x, k*y, k*z)

scale2 :: Int -> Coord -> Coord
scale2 k (x,y) = (k*x, k*y)

mod4 :: Coord4 -> Coord4 -> Coord4
(x,y,z,a) `mod4` (qx,qy,qz,qa) = (x `mod` qx, y `mod` qy, z `mod` qz, a `mod` qa)


manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
manhattan3 :: Coord3 -> Coord3 -> Int
manhattan3 (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)


euclidian :: Coord -> Coord -> Double
euclidian (x1, y1) (x2, y2) = sqrt $ fromIntegral ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2))


clockTurn :: Coord -> Coord
clockTurn (x, y) = (-y, x)
antiTurn :: Coord -> Coord
antiTurn (x, y) = (y, -x)


{-# INLINE neighbourCoords4 #-}
neighbourCoords4 :: [Coord]
neighbourCoords4 = [(1,0), (-1,0), (0,1), (0,-1)]

neighbourCoords6 :: [Coord3]
neighbourCoords6 = [(1,0,0), (-1,0,0), (0,1,0), (0,-1,0), (0,0,1), (0,0,-1)]


neighbourCoords8 :: [Coord]
neighbourCoords8 = [(-1, -1),(0, -1),(1, -1), (1, 0),(1, 1),(0, 1),(-1, 1),(-1, 0)]


{-# INLINE neighbours4 #-}
neighbours4 :: Coord -> [Coord]
neighbours4 c = neighbourCoords4 `at` c

neighbours6 :: Coord3 -> [Coord3]
neighbours6 c = neighbourCoords6 `at3` c


{-# INLINE neighbours8 #-}
neighbours8 :: Coord -> [Coord]
neighbours8 c = neighbourCoords8 `at` c

{-# INLINE nextTo8 #-}
nextTo8 :: Coord -> Coord -> Bool
nextTo8 p q = p `elem` neighbours8 q

{-# INLINE at #-}
at :: [Coord] -> Coord -> [Coord]
coords `at` origin = (+ origin) <$> coords

at3 :: [Coord3] -> Coord3 -> [Coord3]
coords `at3` origin = (+ origin) <$> coords


-- All coords in a grid in (x, y) (col, row) order
allCoords :: Int -> Int -> [Coord]
allCoords rows cols = concatMap (\c -> (c,) <$> [0..(rows-1)]) [0..(cols-1)]


circle :: Int -> Coord -> Int -> Int -> [Coord]
circle distance pos rows cols = filter (\c -> manhattan pos c <= distance) $ allCoords rows cols


directions8 :: [Coord]
directions8 = [(0, -1), (0, 1), (1, 0), (-1, 0), (1, -1), (1, 1), (-1, 1), (-1, -1)]


directions4 :: [Coord]
directions4 = [(0, -1), (0, 1), (1, 0), (-1, 0)]


-- Coordinate start at top left, so y goes up as you go down
leftOf, rightOf, above, below :: Coord -> Coord
leftOf x = x + (-1,0)
rightOf x = x + (1,0)
above x = x + (0,-1)
below x = x + (0,1)


lt, rt, up, dn :: Coord
lt = (-1,0)
rt = (1,0)
up = (0,-1)
dn = (0,1)

lt3, rt3, up3, dn3, in3, ot3 :: Coord3
lt3 = (-1,0,0)
rt3 = (1,0,0)
up3 = (0,-1,0)
dn3 = (0,1,0)
in3 = (0,0,1)
ot3 = (0,0,-1)


levi :: Int -> Int -> Int -> Int
levi 0 1 2 = 1
levi 1 2 0 = 1
levi 2 0 1 = 1
levi 0 2 1 = -1
levi 2 1 0 = -1
levi 1 0 2 = -1
levi i j k
  | i==j || i == k || j == k = 0
  | otherwise = error $ "Error in Levi Civita for i,j,k: " ++ show (i,j,k)


parseGridWith :: (Char -> a) -> [String] -> [(Coord, a)]
parseGridWith  p css = sym
  where
    sym = concatMap (\(y, cs) -> (\(x, c) -> ((x,y), p c)) <$> zip [0..] cs) $ zip [0..] css


-- Basis...
e :: [Coord3]
e = [(1,0,0), (0,1,0), (0,0,1)]

crossProduct :: Coord3 -> Coord3 -> Coord3
crossProduct (v1,v2,v3) (w1,w2,w3) = sum $ (\i -> sum $ (\j -> sum $ (\k -> scale3 (levi i j k * (v!!j) * (w!!k)) (e!!i)) <$> [0..2]) <$> [0..2]) <$> [0..2]
  where
    v = [v1,v2,v3]
    w = [w1,w2,w3]



floodFill :: (Ord a, Show a) => a -> (a -> [a]) -> [a]
floodFill start next = bfs [start] next
  

bfs :: (Ord a, Show a) => [a]-> (a -> [a]) -> [a]
bfs start next = go S.empty (Q.fromList start)
  where
    go !seen = \case
      Q.Empty -> []
      x Q.:< newq
        | x `S.member` seen -> go seen newq
        -- | otherwise -> x : go (x `S.insert` seen) (Q.appendList newq $ next x)
        | otherwise -> go (x `S.insert` seen) (foldr Q.insert newq $ next x)
      _ -> error $ "Not sure it's possible to get here in bfs...: " ++ show seen



dijkstra :: (Ord s, Show s) => (s -> [s]) -> (s -> s -> s) -> (s -> Bool) -> [s] -> Maybe s
dijkstra nextFn costFn finishFn start = go S.empty (Q.fromList start)
  where
    go !visited  = \case
      Q.Empty -> Nothing
      x Q.:< newQ
        | finishFn x -> Just x
        | x `S.member` visited -> go visited newQ 
        | otherwise -> go (x `S.insert` visited) (Q.appendList newQ $ costFn x <$> nextFn x)
      _ -> error $ "Not sure it's possible to get here in dijkstra...: " ++ show visited


dfs :: (Ord a, Show a) => (a -> [a]) -> [a]-> [a]
dfs next start = go S.empty (Q.fromList start)
  where
    go !seen = \case
      Q.Empty -> []
      x Q.:< newq
        | x `S.member` seen -> go seen newq
        -- | otherwise -> x : loop (x `S.insert` seen) (foldl' (\q x -> Q.cons x q) newq (next x))
        | otherwise -> go (x `S.insert` seen) (foldr Q.insert newq $ next x)
      _ -> error $ "Not sure it's possible to get here in bfs...: " ++ show seen


dfsSet :: (Ord a, Show a) => S.Set a-> (a -> S.Set a) -> S.Set a
dfsSet start next = go S.empty (Q.fromList $ S.toList start)
  where
    go !seen = \case
      Q.Empty -> S.empty
      x Q.:< newq
        | x `S.member` seen -> go seen newq
        -- | otherwise -> x `S.insert` loop (x `S.insert` seen) (foldl' (\q x -> Q.cons x q) newq (next x))
        | otherwise -> go (x `S.insert` seen) (foldr Q.insert newq $ next x)
      _ -> error $ "Not sure it's possible to get here in bfs...: " ++ show seen


-- Floyd–Warshall algorithm to detect cycles
isLoop :: Eq a => [a] -> Bool
isLoop a = go a a
  where
   go (x:xs) (_:y:ys) = x == y || go xs ys
   go _      _        = False


-- RECURSION STUFF ---

-- Recursion library
newtype Fix f = Fix { unFix :: f (Fix f) }
type Coalgebra f a = a -> f a
type Algebra f a = f a -> a

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo f g = f . fmap (hylo f g) . g


-- Functor that generates the rose tree
data TreeF a  r = NodeF a [r] deriving (Functor, Show)
data TreeF' a  r = LeafF' | NodeF' a [r] deriving (Functor, Show)
data ListF a b = Nil | Cons a b deriving (Eq,Ord,Show,Read,Functor)


-- Rose tree
type Tree a = Fix (TreeF a)

newtype ForestF a r = ForestF [TreeF a r]


--- Coord 4 stuff

type Coord4 = (Int, Int, Int, Int)

instance Num Coord4 where
  (x1, y1, z1, a1) + (x2, y2, z2, a2) = (x1+x2, y1+y2, z1+z2, a1+a2)
  (x1, y1, z1, a1) - (x2, y2, z2, a2) = (x1-x2, y1-y2, z1-z2, a1-a2)
  (x1, y1, z1, a1) * (x2, y2, z2, a2) = (x1*x2, y1*y2, z1*z2, a1*a2)
  abs (x, y, z, a) = (abs x, abs y, abs z, abs a)
  signum (x, y, z, a) = (signum x, signum y, signum z, signum a)
  fromInteger i = (fromInteger i, 0, 0, 0)


instance (HasTrie a, HasTrie b, HasTrie c, HasTrie d) => HasTrie (a,b,c,d) where
    newtype (a,b,c,d) :->: x = QuadTrie (((a,b,c),d) :->: x)
    trie f = QuadTrie (trie (f . trip))
    untrie (QuadTrie t) = untrie t . detrip
    enumerate (QuadTrie t) = enum' trip t


trip :: ((a,b,c),d) -> (a,b,c,d)
trip ((a,b,c),d) = (a,b,c,d)

detrip :: (a,b,c,d) -> ((a,b,c),d)
detrip (a,b,c,d) = ((a,b,c),d)

enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a', b)]
enum' f = (fmap.first) f . enumerate
