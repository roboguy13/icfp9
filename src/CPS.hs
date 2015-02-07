{-# LANGUAGE TypeOperators, RankNTypes #-}

module CPS
  where

-- | Sum type
type a :| b = forall r. (a -> r) -> (b -> r) -> r

-- | Product type
type a * b = forall r. (a -> b -> r) -> r

type Choice a b = a :| b

test :: Int :| (Char * Int)
        -> Int
test f =
  f id (\g -> g (\x y -> y))

