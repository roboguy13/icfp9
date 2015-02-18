{-# LANGUAGE RankNTypes, MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
module VM.Instruction
  (Instruction
  ,(#)
  ,(.=)
  )
  where

import Memory.Memory
import VM.Type

import Control.Monad.State

import Data.Functor.Contravariant

--type a <-> b = (f a -> g a) -> (f a -> f b, f b -> f a)

type f <-> g = forall a. (f a -> g a, g a -> f a)

{-
class Nat x where
  type Ob x
  (#) :: proxy x -> Ob a -> (a -> Ob b) -> Ob b

instance Monad m => Nat m where
-}


--(#) :: Monad m => m a -> (a -> m b) -> m b
--(#) = undefined

infixl 0 #
--(#) :: ()
(#) :: a -> (a -> b) -> b
(#) = undefined
--x # f = f x

(.=) :: ()
(.=) = undefined

