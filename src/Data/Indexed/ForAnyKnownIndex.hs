{-# LANGUAGE ConstraintKinds
           , MultiParamTypeClasses
           , PolyKinds
           , ScopedTypeVariables
           , TypeOperators
  #-}

module Data.Indexed.ForAnyKnownIndex
  ( ForAnyKnownIndex
  , instAnyKnownIndex
  , ForAnyKnownIndexF
  , instAnyKnownIndexF
  , ForAnyKnownIndex2
  , instAnyKnownIndex2
  )
where

import Data.Constraint ( (:-) )

import GHC.TypeLits ( KnownNat )


class ForAnyKnownIndex p f a
  where instAnyKnownIndex :: forall n
                           . KnownNat n :- p (f n a)


class ForAnyKnownIndexF p f
  where instAnyKnownIndexF :: forall n
                            . KnownNat n :- p (f n)


class ForAnyKnownIndex2 p f a
  where instAnyKnownIndex2 :: forall n m
                            . (KnownNat n, KnownNat m) :- p (f n m a)
