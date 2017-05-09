{-# LANGUAGE ConstraintKinds
           , MultiParamTypeClasses
           , PolyKinds
           , ScopedTypeVariables
           , TypeOperators
  #-}

module Data.Indexed.ForAnyKnownIndex
  ( ForAnyKnownIndex (..)
  , ForAnyKnownIndexF (..)
  , ForAnyKnownIndex2 (..)
  , ForAnyKnownIndex2F (..)
  )
where

import Data.Constraint ( (:-) )

import Data.Indexed.Nat ( KnownNat )


class ForAnyKnownIndex p f a
  where instAnyKnownIndex :: forall n
                           . KnownNat n :- p (f n a)


class ForAnyKnownIndexF p f
  where instAnyKnownIndexF :: forall n
                            . KnownNat n :- p (f n)


class ForAnyKnownIndex2 p f a
  where instAnyKnownIndex2 :: forall n m
                            . (KnownNat n, KnownNat m) :- p (f n m a)


class ForAnyKnownIndex2F p f
  where instAnyKnownIndex2F :: forall n m
                             . (KnownNat n, KnownNat m) :- p (f n m)
