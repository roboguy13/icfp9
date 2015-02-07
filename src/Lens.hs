{-# LANGUAGE RankNTypes #-}

-- A simple, barebones implementation of lenses

module Lens
  (Lens
  ,Lens'
  ,Traversal
  ,Traversal'

  ,over
  ,(%~)

  ,toListOf
  ,(^..)

  ,mconcatOf

  ,set
  ,(.~)

  ,view
  ,(^.)

  ,maybeView
  ,(^?)
  )
  where

import Data.Functor.Identity
import Data.Functor.Contravariant

import Control.Applicative

import Data.Monoid


type Optic f s t a b = (a -> f b) -> s -> f t

type Setter s t a b = Optic Identity s t a b
type Getter s t a b = forall f. (Functor f, Contravariant f) => Optic f s t a b

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a    = Lens s s a a

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a    = Traversal s s a a

over :: Setter s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)
{-# INLINE over #-}

(%~) :: Setter s t a b -> (a -> b) -> s -> t
(%~) = over
{-# INLINE (%~) #-}

toListOf :: Traversal s t a () -> s -> [a]
toListOf traversal = getConst . traversal (\x -> Const [x])
{-# INLINE toListOf #-}

(^..) :: Traversal s t a () -> s -> [a]
(^..) = toListOf
{-# INLINE (^..) #-}

mconcatOf :: Optic (Const w) a t a b -> (a -> w) -> a -> w
mconcatOf l f = getConst . l (Const . f)
{-# INLINE mconcatOf #-}

set :: Setter s t a b -> b -> s -> t
set l = over l . const
{-# INLINE set #-}

(.~) :: Setter s t a b -> b -> s -> t
(.~) = set
{-# INLINE (.~) #-}

view :: Lens s t a b -> s -> a
view l = getConst . l Const
{-# INLINE view #-}

(^.) :: Lens s t a b -> s -> a
(^.) = view
{-# INLINE (^.) #-}

maybeView :: Traversal a t a b -> a -> Maybe a
maybeView l = getFirst . mconcatOf l (First . Just)
{-# INLINE maybeView #-}

(^?) :: Traversal a t a b -> a -> Maybe a
(^?) = maybeView
{-# INLINE (^?) #-}

