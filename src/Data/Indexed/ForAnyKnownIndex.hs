{-# LANGUAGE ConstraintKinds
           , FlexibleContexts
           , FlexibleInstances
           , PolyKinds
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TypeOperators
           , UndecidableInstances
           , UndecidableSuperClasses
  #-}

module Data.Indexed.ForAnyKnownIndex
  ( ForAnyKnownIndex
  , instAnyKnownIndex
  , ForAnyKnownIndex2
  , instAnyKnownIndex2
  )
where

import Data.Constraint ( (:-) (Sub)
                       , Dict (Dict)
                       , (\\)
                       , (:=>) (ins)
                       )
import Data.Constraint.Forall ( Forall
                              , inst
                              , ForallV
                              , instV
                              )

import GHC.TypeLits ( KnownNat )


class (KnownNat n :=> p (f n a)) => IndexLast p f a n
instance (KnownNat n :=> p (f n a)) => IndexLast p f a n

class Forall (IndexLast p f a) => ForAnyKnownIndex p f a
instance Forall (IndexLast p f a) => ForAnyKnownIndex p f a

instAnyKnownIndex :: forall p f n a. KnownNat n => ForAnyKnownIndex p f a :- p (f n a)
instAnyKnownIndex
  = Sub $ Dict \\ (ins :: KnownNat n :- p (f n a))
               \\ (inst :: Forall (IndexLast p f a) :- IndexLast p f a n)


class ((KnownNat n, KnownNat m) :=> p (f n m a)) => IndexLast2 p f a n m
instance ((KnownNat n, KnownNat m) :=> p (f n m a)) => IndexLast2 p f a n m

class ForallV (IndexLast2 p f a) => ForAnyKnownIndex2 p f a
instance ForallV (IndexLast2 p f a) => ForAnyKnownIndex2 p f a

instAnyKnownIndex2 :: forall p f n m a. (KnownNat n, KnownNat m) => ForAnyKnownIndex2 p f a :- p (f n m a)
instAnyKnownIndex2
  = Sub $ Dict \\ (ins :: (KnownNat n, KnownNat m) :- p (f n m a))
               \\ (instV :: ForallV (IndexLast2 p f a) :- IndexLast2 p f a n m)
