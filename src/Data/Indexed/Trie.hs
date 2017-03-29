{-# OPTIONS_GHC -fplugin TypeNatSolver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE AllowAmbiguousTypes
           , DataKinds
           , GADTs
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeApplications
           , TypeOperators
  #-}
module Data.Indexed.Trie
  ( Litrie (..)
  , prune
  )
where

import Data.Indexed.Index ( Index (Index)
                          , switchZero'
                          )

import Data.Indexed.Nat ( KnownNat
                        , type (<=)
                        , type (+), type (-)
                        )


data Litrie n a
  where End :: a -> Litrie 0 a
        Cut :: Litrie n a
        One :: a -> Litrie n a -> Litrie (n + 1) a
        Two :: a -> Litrie n a -> Litrie n a -> Litrie (n + 1) a

deriving instance Show a => Show (Litrie n a)

prune :: forall i n a
       . (KnownNat n, i <= n)
      => Index i () -> (a -> Bool) -> Litrie n a -> Litrie n a
prune Index = prune' @ i

prune' :: forall i n a
        . (KnownNat i, KnownNat n, i <= n)
       => (a -> Bool) -> Litrie n a -> Litrie n a
prune' _ e@(End _) = e
prune' _ Cut = Cut
prune' p o@(One x xs)
  = switchZero' @ i
      ( if p x then Cut else o )
      ( One x $ prune' @ (i - 1) p xs )
