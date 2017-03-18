{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE DataKinds
           , GADTs
           , KindSignatures
           , FlexibleContexts
           , RankNTypes
           , ScopedTypeVariables
           , TypeApplications
           , TypeOperators
  #-}

module Data.Indexed.Capped
  ( Capped (..)
  , relaxCap
  , tryCap
  , withCapped
  )
where

import GHC.TypeLits ( Nat
                    , KnownNat
                    , type (<=)
                    )

import Data.Constraint ( (:-)
                       , (\\)
                       )
import Data.Constraint.Nat ( leTrans )


import Data.Indexed.ForAnyKnownIndex ( ForAnyKnownIndex
                                     , instAnyKnownIndex
                                     , ForAnyKnownIndexF
                                     , instAnyKnownIndexF
                                     )

import Data.Indexed.Some ( Some (Some) )

import Data.Indexed.Index ( index'
                          , IsLessOrEqual (LessOrEqual, GreaterThan)
                          , orderIndex'
                          )


data Capped l f a
  where Capped :: (n <= l, KnownNat n)
               => f (n :: Nat) a -> Capped l f a


instance (KnownNat l, ForAnyKnownIndex Show f a) => Show (Capped l f a)
  where showsPrec p (Capped (x :: f n a))
          = showParen (p > appPrec)
              ( showString "Capped @ "
              . showsPrec (appPrec + 1) (index' @ l)
              . showString " "
              . showsPrec (appPrec + 1) x
              )
              \\ (instAnyKnownIndex :: KnownNat n :- Show (f n a))
          where appPrec = 10


instance ForAnyKnownIndexF Functor f => Functor (Capped l f)
  where fmap f (Capped (x :: f n a))
          = Capped (fmap f x)
              \\ (instAnyKnownIndexF :: KnownNat n :- Functor (f n))


instance ForAnyKnownIndexF Foldable f => Foldable (Capped l f)
  where foldMap f (Capped (x :: f n a))
          = foldMap f x
              \\ (instAnyKnownIndexF :: KnownNat n :- Foldable (f n))


instance ( ForAnyKnownIndexF Functor f
         , ForAnyKnownIndexF Foldable f
         , ForAnyKnownIndexF Traversable f
         ) => Traversable (Capped l f)
  where traverse f (Capped (x :: f n a))
          = Capped <$> traverse f x
              \\ (instAnyKnownIndexF :: KnownNat n :- Traversable (f n))


withCapped :: (forall n. (n <= l, KnownNat n) => f n a -> r) -> (Capped l f a -> r)
withCapped f (Capped x) = f x


tryCap :: forall cap f a. KnownNat cap => Some f a -> Maybe (Capped cap f a)
tryCap (Some (x :: f n a))
  = case orderIndex' @ n @ cap
      of LessOrEqual -> Just $ Capped x
         GreaterThan -> Nothing


relaxCap :: forall l m f a. (l <= m) => Capped l f a -> Capped m f a
relaxCap (Capped (x :: f n a))
  = Capped x
      \\ leTrans @ n @ l @ m
