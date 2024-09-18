module Queue where

------------------------------------------------------------------------

newtype Queue a = Queue [a]

emptyQueue :: Queue a
emptyQueue = Queue []

enqueue :: Queue a -> a -> Queue a
enqueue (Queue xs) x = Queue (xs ++ [x])

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [])       = Nothing
dequeue (Queue (x : xs)) = Just (x, Queue xs)

------------------------------------------------------------------------

enqueue' :: Queue a -> a -> Queue a
enqueue' (Queue xs) x = Queue (x : xs)

dequeue' :: Queue a -> Maybe (a, Queue a)
dequeue' (Queue [])       = Nothing
dequeue' (Queue (x : xs)) = Just (x, Queue (xs ++ [x]))

------------------------------------------------------------------------

newtype PQueue a = PQueue [a]
  deriving Show

emptyPQueue :: PQueue a
emptyPQueue = PQueue []

isEmptyPQueue :: PQueue a -> Bool
isEmptyPQueue (PQueue []) = True
isEmptyPQueue _           = False

lengthPQueue :: PQueue a -> Int
lengthPQueue (PQueue xs) = length xs

insertBy :: (a -> a -> Ordering) -> a -> PQueue a -> PQueue a
insertBy p x (PQueue xs0) = PQueue (go xs0)
  where
    go []       = [x]
    go (y : xs) = case p x y of
      LT -> x : y : xs
      EQ -> x : y : xs
      GT -> y : go xs

peek :: Int -> PQueue a -> a
peek ix (PQueue xs) = xs !! ix

