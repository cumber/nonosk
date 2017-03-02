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
  )
where

import Data.Constraint ( (:-) (Sub)
                       , Dict (Dict)
                       , (\\)
                       )
import Data.Constraint.Forall ( Forall
                              , inst
                              )

class p (f n a) => IndexLast p f a n
instance p (f n a) => IndexLast p f a n

class Forall (IndexLast p f a) => ForallIndex p f a
instance Forall (IndexLast p f a) => ForallIndex p f a

instAnyIndex :: forall p f n a. ForallIndex p f a :- p (f n a)
instAnyIndex
  = Sub $ Dict \\ (inst :: Forall (IndexLast p f a) :- IndexLast p f a n)
