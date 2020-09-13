module Main where

import Criterion.Main
import Debug.Trace

infixl 9 !?

{-# INLINEABLE (!?) #-}
(!?) :: (Foldable f) => f a -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise =
    foldr
      ( \x r k ->
          case k of
            0 -> Just x
            _ -> r (k -1)
      )
      (const Nothing)
      xs
      n

myList :: [Int]
myList = trace "myList was evaluated" [1 .. 9999]

benchList :: IO ()
benchList =
  defaultMain
    [ bench "index list 9999" $
        whnf (myList !!) 9998,
      bench "index list  maybe 9999" $
        whnf (myList !?) 9999,
      bench "map list 9999" $
        nf (map (+ 1)) myList
    ]

{- ============================================= -}
{- DList -}
newtype DList a = DL {unDL :: [a] -> [a]}

empty :: DList a
empty = DL $ id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton = DL . (:)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList = ($ []) . unDL
{-# INLINE toList #-}

infixr 9 `cons`

cons :: a -> DList a -> DList a
cons x xs = DL ((x :) . unDL xs)
{-# INLINE cons #-}

infixl 9 `snoc`

snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x :))
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append (DL a) (DL b) = DL $ b . a
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    go n xs = go (n -1) (xs ++ [n])

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs = go (n -1) (xs `append` singleton n)

benchDlist :: IO ()
benchDlist =
  defaultMain
    [ bench "concat list" $
        nf schlemiel 1234,
      bench "concat dlist" $
        nf constructDlist 1234
    ]

{- ============================================= -}
{- Queue -}

data Queue a = Queue
  { enqueue :: [a],
    dequeue :: [a]
  }
  deriving (Eq, Show)

push :: a -> Queue a -> Queue a
push a (Queue f r) = Queue f (a : r)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue [] r) = pop $ Queue (reverse r) []
pop (Queue (f : []) r) = Just $ (f, Queue (reverse r) [])
pop (Queue (f : fs) r) = Just $ (f, Queue fs r)

listPushPop :: [Int] -> [Int]
listPushPop = f . f . f
  where
    f = pop' . (push' 1)
    push' x xs = xs ++ [x]
    pop' (x : xs) = xs

queuePushPop :: Queue Int -> Queue Int
queuePushPop = f . f . f
  where
    f = pop' . (push 1)
    pop' q' = case pop q' of
      Nothing -> Queue [] []
      Just (_, a) -> a

benchQueue :: IO ()
benchQueue =
  defaultMain
    [ bench "pushpop list" $
        whnf listPushPop l,
      bench "pushpop queue" $
        whnf queuePushPop q
    ]
  where
    l = [0 .. 999]
    q = Queue [0 .. 499] [999 .. 500]

main :: IO ()
main = benchQueue