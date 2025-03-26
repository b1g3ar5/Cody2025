{-# language TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grid where

import Control.Comonad (Comonad(..))
import Data.Maybe (fromJust, isJust)
import Data.Distributive (Distributive(distribute))
import Data.Functor.Compose (Compose(..))
import Data.Functor.Foldable (Base, Corecursive(ana, embed), Recursive(cata, project))

-- Conceptually, forward and backward streams are different,
-- so here I separated them into two data types

-- Infinite stream
data Stream a = (:>) { headS :: a, tailS :: Stream a } deriving Functor

infixr 5 :>

-- All recursions will be done using recursion schemes

-- Recursion schemes for streams
data FPair a x = P a x deriving Functor

type instance Base (Stream a) = FPair a

-- We can use cata (fold)
instance Recursive (Stream a) where
    project :: Stream a -> Base (Stream a) (Stream a)
    project (a :> as) = P a as

-- We can use ana (unfold)
instance Corecursive (Stream a) where
    embed :: Base (Stream a) (Stream a) -> Stream a
    embed (P a as) = a :> as


-- Zippy applicative
instance Applicative Stream where
    pure :: a -> Stream a
    pure = ana (\a -> P a a) -- infinite stream of a's
    (<*>) :: Stream (a -> b) -> Stream a -> Stream b
    fs <*> as = fmap (uncurry ($)) (zipS fs as)


zipS :: Stream a -> Stream b -> Stream (a, b)
zipS as bs = ana (\(as, bs) -> P (headS as, headS bs) (tailS as, tailS bs)) (as, bs)


-- Stream is distributive over any functor
instance Distributive Stream where
    distribute :: Functor f => f (Stream a) -> Stream (f a)
    distribute = ana (\fStms -> P (headS <$> fStms) (tailS <$> fStms))


instance Show a => Show (Stream a) where
    show :: Stream a -> String
    show = show . take 4 . toInfList


-- Backward infinite stream
data BStream a = (:<) { tailBS :: BStream a, headBS :: a } deriving Functor

infixl 5 :<

data BPair a x = BP x a deriving Functor

type instance Base (BStream a) = BPair a

instance Corecursive (BStream a) where
    embed :: Base (BStream a) (BStream a) -> BStream a
    embed (BP as a) = as :< a

-- Zippy applicative
instance Applicative BStream where
    pure :: a -> BStream a
    pure = ana (\a -> BP a a) -- infinite stream of a's
    (<*>) :: BStream (a -> b) -> BStream a -> BStream b
    fs <*> as = fmap (uncurry ($)) (zipBS fs as)

zipBS :: BStream a -> BStream b -> BStream (a, b)
zipBS as bs = ana (\(as, bs) -> BP (tailBS as, tailBS bs) (headBS as, headBS bs))(as, bs)


instance Distributive BStream where
    distribute :: Functor f => f (BStream a) -> BStream (f a)
    --distribute fStms = distribute (tailBS <$> fStms) :< (headBS <$> fStms)
    distribute = ana (\fStms -> BP (tailBS <$> fStms) (headBS <$> fStms))

-- Forward and backward iterate
iterateS :: (a -> a) -> a -> Stream a
iterateS f = ana (\a -> P a (f a))

iterateB :: (a -> a) -> a -> BStream a
iterateB f = ana (\x -> BP (f x) x)

-- List conversions

toInfList :: Stream a -> [a]
toInfList = cata (\(P a as) -> a : as)


-- from stream of Maybe's (terminated by Nothing)
stmToList :: Stream (Maybe b) -> [b]
stmToList = fmap fromJust . takeWhile isJust . toInfList


-- from stream of lists (terminated by an empty list)
stmToMatrix :: Stream [a] -> [[a]]
stmToMatrix = takeWhile (not . null) . toInfList


-- Pointer to a location in an infinite bidirectional stream
-- Separating current value makes this definition more symmetric
data Cursor a = Cur { bwStm :: BStream a, cur :: a, fwStm :: Stream a } deriving Functor


instance Applicative Cursor where
    pure :: a -> Cursor a
    pure a = Cur (pure a) a (pure a)
    (<*>) :: Cursor (a -> b) -> Cursor a -> Cursor b
    Cur fbw f ffw <*> Cur fw a bw = Cur (fbw <*> fw) (f a) (ffw <*> bw)


instance Distributive Cursor where
    distribute :: Functor f => f (Cursor a) -> Cursor (f a)
    distribute fCur = Cur (distribute (bwStm <$> fCur))
                          (cur <$> fCur)
                          (distribute (fwStm <$> fCur))


instance Comonad Cursor where
    extract :: Cursor a -> a
    duplicate :: Cursor a -> Cursor (Cursor a)
    extract (Cur _ a _) = a
    duplicate cur = Cur (iterateB moveBwd (moveBwd cur))
                         cur
                        (iterateS moveFwd (moveFwd cur))


-- move the cursor
moveFwd :: Cursor a -> Cursor a
moveFwd (Cur bw a (x :> fw)) = Cur (bw :< a) x fw


moveBwd :: Cursor a -> Cursor a
moveBwd (Cur (bw :< x) a fw) = Cur bw x (a :> fw)


curToList :: Cursor (Maybe a) -> [a]
curToList (Cur _ Nothing as) = []
curToList (Cur _ (Just a) as) = a : stmToList as


listToCur :: (a -> b) -> b -> [a] -> Cursor b
listToCur _ padding [] = pure padding
listToCur convert padding (a : as) = Cur (pure padding) (convert a) (stmFromList as)
    where stmFromList = foldr ((:>) . convert) (pure padding)


------------------------------
-- A grid: a cursor of cursors
------------------------------

type Grid a = Compose Cursor Cursor a

instance (Comonad w2, Comonad w1, Distributive w1) => Comonad (Compose w2 w1) where
    extract :: Compose w2 w1 a -> a
    extract = extract . extract . getCompose
    duplicate :: Compose w2 w1 a -> Compose w2 w1 (Compose w2 w1 a)
    duplicate = fmap Compose . Compose .
                fmap distribute . duplicate . fmap duplicate .
                getCompose

instance Show a => Show (Cursor a) where
    show :: Cursor a -> String
    show (Cur _ a (b :> c :> _)) = show a ++ show b ++ show c ++ "\n"

instance {-# OVERLAPPING #-} Show a => Show (Grid a) where
  show :: Show a => Grid a -> String
  show = show . getCompose


matrixToGrid :: [[a]] -> Grid (Maybe a)
matrixToGrid = Compose . listToCur id (pure Nothing) .
                         fmap (listToCur Just Nothing)


gridToMatrix :: Grid (Maybe a) -> [[a]]
gridToMatrix (Compose curcur) = stmToMatrix (a :> as)
  where (Cur _ a as) = curToList <$> curcur


-- Directions: Z is stay in place
data Dir = B | Z | F deriving (Eq, Enum, Bounded, Show)


-- (x, y) direction
type Dir2 = (Dir, Dir)


allDirs :: [Dir2]
allDirs = [(h, v) | h <- [minBound .. maxBound]
                  , v <- [minBound .. maxBound]
                  , (h, v) /= (Z, Z)]


moveCur :: Dir -> Cursor a -> Cursor a
moveCur dir =
    case dir of
        B -> moveBwd
        Z -> id
        F -> moveFwd


-- move all rows in the same direction
moveH :: Dir -> Grid a -> Grid a
moveH dir (Compose (Cur  up cur dn)) = Compose $ Cur (mv <$> up) (mv cur) (mv <$> dn)
    where mv = moveCur dir


move2 :: Dir2 -> Grid a -> Grid a
move2 (h, v) = moveH h .  Compose . moveCur v . getCompose


peek :: Dir2 -> Grid a -> a
peek dir = extract . move2 dir


{-
-- First task

-- return Nothing if we're starting outside of bounds
matchesAround :: Eq a => [a] -> Grid (Maybe a) -> Maybe Int
matchesAround [] _ = error "Empty list in matchesAround"
matchesAround (a : as) grid
  | isNothing mx = Nothing
  | fromJust mx == a = Just $ length $ filter (matchSuffix as grid) allDirs
  | otherwise = Just 0
  where
    mx = extract grid
    

matchSuffix :: Eq a => [a] -> Grid (Maybe a) -> Dir2 -> Bool
matchSuffix as grid dir = mas `isPrefixOf` mxs
  where
    grid' = move2 dir grid
    mas = fmap Just as
    mx = extract grid'
    mxs = walkFrom grid' dir mx
        

-- walk the grid and acuumulate values
-- given the direction and the starting value
walkFrom :: Grid a -> Dir2 -> a -> [a]
walkFrom grid dir a =  fst <$> iterate (go dir) (a, move2 dir grid)
  where 
    go :: Dir2 -> (a, Grid a) -> (a, Grid a)
    go d (_, g) = (extract g, move2 d g)


solve1 :: Grid (Maybe Char) -> Int
solve1 grid = sum $ sum <$> sums
  where
    matches = extend (matchesAround "XMAS") grid
    sums = gridToMatrix matches

--- Second task

xMas :: Grid (Maybe Char) -> Maybe Bool
xMas grid = case extract grid of
    Nothing  -> Nothing
    Just 'A' -> Just $ diag grid (Just 'M', Just 'S')
    _        -> Just False

diag :: Eq a => Grid a -> (a, a) -> Bool
diag grid p = (p == x1 || p == swap x1) && (p == x2 || p == swap x2)
  where
    x1 = ( peek (F, F) grid, peek (B, B) grid)
    x2 = ( peek (F, B) grid, peek (B, F) grid)

solve2 :: Grid (Maybe Char) -> Int
solve2 grid = sum $ fmap (length . filter id) matches
  where
    matches = gridToMatrix (extend xMas grid)
{-
day4_BM :: IO ()
day4_BM = do
    txt <- getLines 4
    let grid = matrixToGrid $ txt
    print $ solve1 grid
    print $ solve2 grid
-}
----------
-- Testing
----------
-- Laws
testGrid :: Grid (Maybe Integer)
testGrid = matrixToGrid [[1,2],[3,4]]

instance Show a => Show (Cursor a) where
    show :: Cursor a -> String
    show (Cur _ a (b :> c :> _)) = show a ++ show b ++ show c ++ "\n"

-- extract . duplicate = id
law1 :: [[Integer]]
law1 = gridToMatrix $ (extract . duplicate) testGrid
-- fmap extract . duplicate = id
law2 :: [[Integer]]
law2 = gridToMatrix $  (fmap extract . duplicate) testGrid
-- duplicate . duplicate = fmap duplicate . duplicate
law3 :: Compose Cursor Cursor (Compose Cursor Cursor [[Integer]])
law3 = fmap gridToMatrix <$> (duplicate . duplicate) testGrid
law4 :: Compose Cursor Cursor (Compose Cursor Cursor [[Integer]])
law4 = fmap gridToMatrix <$> (fmap duplicate . duplicate) testGrid

test1, test2 :: Int
test1 = solve1 $ matrixToGrid test
test2 = solve2 $ matrixToGrid test

-- answer: XMAS 18 times
test :: [String]
test = [
    "MMMSXXMASM",
    "MSAMXMSMSA",
    "AMXSXMAAMM",
    "MSAMASMSMX",
    "XMASAMXAMM",
    "XXAMMXXAMA",
    "SMSMSASXSS",
    "SAXAMASAAA",
    "MAMMMXMMMM",
    "MXMXAXMASX"]  

-}  