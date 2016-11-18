module FingerTree where

import Data.Monoid

data Tree v a = Leaf v a
              | Branch v (Tree v a) (Tree v a)
              deriving (Eq, Show)

type Size = Int

type Priority = Int

toList :: Tree v a -> [a]
toList (Leaf _ a) = [a]
toList (Branch _ l r) = toList l ++ toList r

tag :: Tree v a -> v
tag (Leaf v _) = v
tag (Branch v _ _) = v

myHead :: Tree v a -> a
myHead (Leaf _ a) = a
myHead (Branch _ l _) = myHead l

leaf :: a -> Tree Size a
leaf a = Leaf 1 a

branch :: Tree Size a -> Tree Size a -> Tree Size a
branch l r = Branch (tag l + tag r) l r

nth :: Tree Size a -> Int -> a
nth (Leaf _ a) 0 = a
nth (Branch _ l r) n
  | n < tag l = nth l n
  | otherwise = nth r (n - tag l)

winner :: Tree Priority a -> a
winner t = go t
  where
    go (Leaf _ a) = a
    go (Branch _ l r)
      | tag l == tag t = go l
      | tag r == tag t = go r
