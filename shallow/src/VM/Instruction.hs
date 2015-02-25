{-# LANGUAGE RankNTypes, MultiParamTypeClasses, TypeFamilies, TypeOperators, FunctionalDependencies #-}
module VM.Instruction
  (Instruction
  ,(#)
  )
  where

import VM.Type

infixl 0 #
--(#) :: a -> (a -> b) -> b
--x # f = f x
(#) :: Monad m => m a -> (a -> m b) -> m b
(#) = (>>=)

