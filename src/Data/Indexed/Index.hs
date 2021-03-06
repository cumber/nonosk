{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise
                -fplugin GHC.TypeLits.KnownNat.Solver
                -fplugin TypeNatSolver
  #-}
{-# LANGUAGE AllowAmbiguousTypes
           , ConstraintKinds
           , DataKinds
           , FlexibleInstances
           , GADTs
           , MagicHash
           , MultiParamTypeClasses
           , RankNTypes
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeApplications
           , TypeFamilies
           , TypeOperators
  #-}

module Data.Indexed.Index
  ( Index (..)
  , IsZero (..)
  , index, index'
  , indexOf
  , succ, pred
  , sameIndex, sameIndex'
  , withIndexOf
  , withSameIndex
  , isZero
  , switchZero, switchZero'
  , IsLessOrEqual (..)
  , orderIndex, orderIndex'
  , nTimes, nTimes'
  )
where

import Prelude hiding ( succ, pred )

import Data.Constraint ( Dict (Dict)
                       , (:-) (Sub)
                       )

import Data.Type.Equality ( (:~:) (Refl) )

import GHC.Prim ( proxy# )

import GHC.TypeLits ( natVal' )

import Numeric.Natural ( Natural )

import Unsafe.Coerce ( unsafeCoerce )


import Data.Indexed.ForAnyKnownIndex ( ForAnyKnownIndex (instAnyKnownIndex) )

import Data.Indexed.Nat ( KnownNat
                        , type (<=), type (>=)
                        , type (>)
                        , type (+), type (-)
                        )


data Index n i
  where Index :: KnownNat n => Index n ()


index :: forall n. Index n () -> Natural
index Index = index' @ n


index' :: forall n. KnownNat n => Natural
index' = fromIntegral $ natVal' (proxy# @ n)

indexOf :: KnownNat n => f n a -> Index n ()
indexOf _ = Index


instance Show (Index n ())
  where showsPrec p i
          = showParen (p > appPrec)
              ( showString "Index @ "
              . showsPrec (appPrec + 1) (index i)
              )
          where appPrec = 10

instance ForAnyKnownIndex Show Index ()
  where instAnyKnownIndex = Sub Dict


succ :: Index n () -> Index (n + 1) ()
succ Index = Index

pred :: (n >= 1) => Index n () -> Index (n - 1) ()
pred Index = Index


sameIndex :: Index n () -> Index m () -> Maybe (n :~: m)
sameIndex Index Index = sameIndex'

sameIndex' :: forall n m. (KnownNat n, KnownNat m) => Maybe (n :~: m)
sameIndex'
  | index' @ n == index' @ m
    = Just $ unsafeCoerce (Refl :: 0 :~: 0)
  | otherwise
    = Nothing

withIndexOf :: KnownNat m => Index n () -> (f n a -> r) -> (f m a -> Maybe r)
withIndexOf i@Index f = withSameIndex (const f) i

withSameIndex :: forall f n a g m b r
               . (KnownNat n, KnownNat m)
              => (f n a -> g n b -> r) -> (f n a -> g m b -> Maybe r)
withSameIndex f x y = case sameIndex' @ n @ m
                      of Just Refl -> Just $ f x y
                         Nothing   -> Nothing


data IsZero n
  where Zero :: (n ~ 0) => IsZero n
        NonZero :: (n > 0) => IsZero n

deriving instance Show (IsZero n)


isZero :: Index n () -> IsZero n
isZero n = switchZero n Zero NonZero


switchZero :: forall n r. Index n () -> ((n ~ 0) => r) -> ((n >= 1) => r) -> r
switchZero Index = switchZero' @ n

switchZero' :: forall n r. KnownNat n => ((n ~ 0) => r) -> ((n >= 1) => r) -> r
switchZero' zero nonzero
  = case orderIndex' @ n @ 0
      of LessOrEqual -> zero
         GreaterThan -> nonzero


data IsLessOrEqual n m
  where LessOrEqual :: (n <= m) => IsLessOrEqual n m
        GreaterThan :: (n > m) => IsLessOrEqual n m


instance (KnownNat n, KnownNat m) => Show (IsLessOrEqual n m)
  where showsPrec p v
          = showParen (p > appPrec)
              ( showsCons v
              . showString " @ "
              . showsPrec (appPrec + 1) (index' @ n)
              . showString " @ "
              . showsPrec (appPrec + 1) (index' @ m)
              )
          where appPrec = 10
                showsCons :: IsLessOrEqual x y -> ShowS
                showsCons LessOrEqual = showString "LessOrEqual"
                showsCons GreaterThan = showString "GreaterThan"


orderIndex :: Index n () -> Index m () -> n `IsLessOrEqual` m
orderIndex Index Index = orderIndex'

orderIndex' :: forall n m. (KnownNat n, KnownNat m) => n `IsLessOrEqual` m
orderIndex'
  | index' @ n <= index' @ m
    = case unsafeCoerce (Dict :: Dict (0 <= 1))
        of (Dict :: Dict (n <= m)) -> LessOrEqual
  | otherwise
    = case unsafeCoerce (Dict :: Dict (1 > 0))
        of (Dict :: Dict (n > m)) -> GreaterThan


nTimes :: forall count f n a
        .    Index count ()
          -> (forall m. f m a -> f (m + 1) a)
          -> f n a
          -> f (n + count) a
nTimes i f x
  = switchZero i x (nTimes (pred i) f (f x))

nTimes' :: forall count f n a
         . KnownNat count
        =>    (forall m. f m a -> f (m + 1) a)
           -> f n a
           -> f (n + count) a
nTimes' = nTimes Index
