{-# LANGUAGE ScopedTypeVariables #-}

module Generate.Tree where

import Data.List
import Data.Maybe

------------------------------------------------------------------------

data Tree a = Node { root :: a , subtrees :: [Tree a] }
  deriving Show

instance Functor Tree where
  fmap f (Node x xs) = Node (f x) (map (fmap f) xs)

singleton :: a -> Tree a
singleton x = Node x []

unfoldTree :: forall a. (a -> [a]) -> a -> Tree a
unfoldTree f = go
  where
    go :: a -> Tree a
    go x = Node x $ map go (f x)

-- | Prune any subtrees whose root does not satisfy the predicate
filterTree_ :: forall a. (a -> Bool) -> Tree a -> Maybe (Tree a)
filterTree_ p = go
  where
    go :: Tree a -> Maybe (Tree a)
    go (Node x xs)
      | p x       = Just $ Node x (mapMaybe go xs)
      | otherwise = Nothing

-- | Remove any elements from the tree that do not satisfy the predicate
filterTree :: forall a. (a -> Bool) -> Tree a -> [Tree a]
filterTree p = go
  where
    go :: Tree a -> [Tree a]
    go (Node x xs)
      | p x       = [Node x (concatMap go xs)]
      | otherwise = concatMap go xs

{-------------------------------------------------------------------------------
  @(Tree, singleton, interleave)@ forms an applicative functor, with 'singleton'
  playing the role of 'pure' and 'interleave' playing the role of '(<*>)'.

  We have to satisfy the following three properties:

  > pure f <*> x    == f     <$> x
  > f <*> pure x    == ($ x) <$> f
  > g <*> (f <*> x) == ((.) <$> g <*> f) <*> x

  For comparison, for pure functions the same three properties boil down to

  > f x     == f x       (this property becomes trivial)
  > f x     == ($ x) f
  > g (f x) == (g . f) x

-------------------------------------------------------------------------------}

interleave :: Tree (a -> b) -> Tree a -> Tree b
interleave l@(Node f ls) r@(Node x rs) =
    Node (f x) $ concat [
        [ interleave l' r  | l' <- ls ]
      , [ interleave l  r' | r' <- rs ]
      ]

renderTree :: Tree String -> String
renderTree = intercalate "\n" . go
  where
    go :: Tree String -> [String]
    go (Node x xs) = x : concatMap inset (mark (map go xs))

    inset :: Marked [String] -> [String]
    inset (Marked marked _isFirst isLast) =
        case (isLast, marked) of
          (_,     []  ) -> error "inset: empty list"
          (True,  s:ss) -> ("└─ " ++ s) : map ("   " ++) ss
          (False, s:ss) -> ("├─ " ++ s) : map ("│  " ++) ss

data Marked a = Marked { _marked :: a , _isFirst :: Bool , _isLast :: Bool }
  deriving (Show)

mark :: [a] -> [Marked a]
mark xs0 = case xs0 of
    []     -> []
    [x]    -> [Marked x True True]
    (x:xs) -> Marked x True False : go xs
  where
    go :: [a] -> [Marked a]
    go []     = error "mark: empty list"
    go [x]    = [Marked x False True]
    go (x:xs) = Marked x False False : go xs
