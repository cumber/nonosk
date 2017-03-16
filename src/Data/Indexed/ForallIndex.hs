{-# LANGUAGE ConstraintKinds
           , FlexibleInstances
           , PolyKinds
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TypeOperators
           , UndecidableInstances
           , UndecidableSuperClasses
  #-}

module Data.Indexed.ForallIndex
  ( ForallIndex
  , instAnyIndex
  , ForallIndex2
  , instAnyIndex2
  )
where

import Data.Constraint ( (:-) (Sub)
                       , Dict (Dict)
                       , (\\)
                       )
import Data.Constraint.Forall ( Forall
                              , inst
                              , ForallV
                              , instV
                              )


class p (f n a) => IndexLast p f a n
instance p (f n a) => IndexLast p f a n

class Forall (IndexLast p f a) => ForallIndex p f a
instance Forall (IndexLast p f a) => ForallIndex p f a

instAnyIndex :: forall p f n a. ForallIndex p f a :- p (f n a)
instAnyIndex
  = Sub $ Dict \\ (inst :: Forall (IndexLast p f a) :- IndexLast p f a n)


class p (f n m a) => IndexLast2 p f a n m
instance p (f n m a) => IndexLast2 p f a n m

class ForallV (IndexLast2 p f a) => ForallIndex2 p f a
instance ForallV (IndexLast2 p f a) => ForallIndex2 p f a

instAnyIndex2 :: forall p f n m a. ForallIndex2 p f a :- p (f n m a)
instAnyIndex2
  = Sub $ Dict \\ (instV :: ForallV (IndexLast2 p f a) :- IndexLast2 p f a n m)
