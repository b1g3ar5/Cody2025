{-# Language PatternSynonyms, ViewPatterns #-}

module Heap where

--import Data.Foldable (Foldable(foldl')) 
--import qualified Data.Map.Strict as M
--import Data.Map.Strict (Map)


data Heap a = EmptyHeap | Heap a (Heap a) (Heap a) deriving (Eq, Ord, Functor, Foldable, Show)


isEmpty :: Heap a -> Bool
isEmpty EmptyHeap = True
isEmpty _ = False


null :: Heap a -> Bool
null = isEmpty


size :: Heap a -> Int
size EmptyHeap = 0
size (Heap _ l r) = 1 + size l + size r


singleton :: Ord a => a -> Heap a
singleton x = Heap x EmptyHeap EmptyHeap


union :: Ord a => Heap a -> Heap a -> Heap a
EmptyHeap `union` t2 = t2
t1 `union` EmptyHeap = t1
t1@(Heap x1 l1 r1) `union` t2@(Heap x2 l2 r2)
  | x1 == x2 = Heap (min x1 x2) (l2 `union` r2 `union` r1) l1
  | x1 < x2 = Heap x1 (t2 `union` r1) l1
  | otherwise = Heap x2 (t1 `union` r2) l2


insert :: Ord a => Heap a -> a -> Heap a
insert heap x = singleton x `union` heap


fromList :: Ord a => [a] -> Heap a
fromList = foldl' insert EmptyHeap


toList :: Heap a -> [a]
toList t = inorder t []
  where
    inorder :: Heap a -> [a] -> [a]
    inorder EmptyHeap xs = xs
    inorder (Heap x l r) xs = inorder l (x : inorder r xs)


appendList :: Ord a => Heap a -> [a] -> Heap a
appendList h l = h `union` fromList l


extractMin :: Ord a => Heap a -> Maybe (a, Heap a)
extractMin EmptyHeap = Nothing
extractMin (Heap x l r) = Just (x, l `union` r)


pattern (:<) :: Ord a => a -> Heap a -> Heap a
pattern x :< xs <- (extractMin -> Just (x, xs))


deleteMin :: Ord a => Heap a -> Maybe (Heap a)
deleteMin h = snd <$> extractMin h


