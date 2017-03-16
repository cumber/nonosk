{-# OPTIONS_GHC -fplugin=TypeNatSolver #-}
{-# LANGUAGE AllowAmbiguousTypes
           , ConstraintKinds
           , DataKinds
           , FlexibleInstances
           , GADTs
           , MagicHash
           , RankNTypes
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeApplications
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances
  #-}

module Data.Indexed.Index
  ( Index (..)
  , IsZero (..)
  , index
  , index'
  , someIndex
  , isZero
  , switchZero
  , IsLessOrEqual (..)
  , orderIndex
  , orderIndex'
  )
where

import Data.Constraint ( Dict (Dict) )

import Data.Proxy ( Proxy (Proxy) )

import Data.Semigroup ( (<>) )

import GHC.Prim ( proxy# )

import GHC.Natural ( Natural )

import GHC.TypeLits ( Nat
                    , KnownNat
                    , natVal'
                    , SomeNat (SomeNat)
                    , someNatVal
                    )

import Unsafe.Coerce ( unsafeCoerce )


import Data.Indexed.Nat ( type (<=)
                        , type (>=)
                        , type (>)
                        )

import Data.Indexed.Some ( Some (Some) )


data Index n i
  where Index :: KnownNat n => Index n ()


index :: forall n. Index n () -> Natural
index Index = index' @ n


index' :: forall n. KnownNat n => Natural
index' = fromIntegral $ natVal' (proxy# @ Nat @ n)


someIndex :: Natural -> Some Index ()
someIndex x
  = case someNatVal . fromIntegral $ x
       of Just (SomeNat (Proxy :: Proxy n)) -> Some (Index @ n)
          Nothing -> error $ "Impossible: Natural " <> show x
                               <> " with no corresponding Nat"


instance Show (Index n ())
  where showsPrec p i
          = showParen (p > appPrec)
              ( showString "Index @ "
              . showsPrec (appPrec + 1) (index i)
              )
          where appPrec = 10


data IsZero n
  where Zero :: (n ~ 0) => IsZero n
        NonZero :: (n > 0) => IsZero n

deriving instance Show (IsZero n)


isZero :: Index n () -> IsZero n
isZero n = switchZero n Zero NonZero


switchZero :: forall n r. Index n () -> ((n ~ 0) => r) -> ((n >= 1) => r) -> r
switchZero n zero nonzero
  = case orderIndex n (Index @ 0)
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
