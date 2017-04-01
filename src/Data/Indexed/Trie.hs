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
  , paths
  , prune
  )
where

import Data.Semigroup ( (<>) )


import Data.Indexed.Index ( Index (Index)
                          , switchZero'
                          )

import Data.Indexed.Nat ( KnownNat
                        , type (<)
                        , type (+), type (-)
                        )

import Data.Indexed.Vector ( Vector ((:^), Nil) )


data Link n a
  where Link :: a -> Litrie n a -> Link (n + 1) a

deriving instance Show a => Show (Link n a)


data Litrie n a
  where End :: Litrie 0 a
        Cut :: Litrie n a
        Step :: !(Link n a) -> Litrie n a
        Fork :: !(Link n a) -> !(Link n a) -> Litrie n a

deriving instance Show a => Show (Litrie n a)


paths :: Litrie n a -> [Vector n a]
paths End = [Nil]
paths Cut = []
paths (Step (Link x t)) = (x :^) <$> paths t
paths (Fork (Link x xt) (Link y yt))
  =    ((x :^) <$> paths xt)
    <> ((y :^) <$> paths yt)


prune :: forall i n a
       . (KnownNat n, i < n)
      => Index i () -> (a -> Bool) -> Litrie n a -> Litrie n a
prune Index = prune' @ i

prune' :: forall i n a
        . (KnownNat i, KnownNat n, i < n)
       => (a -> Bool) -> Litrie n a -> Litrie n a
prune' p t
  = switchZero' @ i
      (pruneRoot p t)
      (push (prune' @ (i - 1) @ (n - 1) p) t)


pruneRoot :: (a -> Bool) -> Litrie n a -> Litrie n a
pruneRoot p t
  = case t
      of End -> End
         Cut -> Cut
         Step (Link x _)
           | p x        -> Cut
           | otherwise  -> t
         Fork xl@(Link x _) yl@(Link y _)
           | p x        -> pruneRoot p (Step yl)
           | p y        -> Step xl
           | otherwise  -> t


push :: (Litrie n a -> Litrie n a) -> (Litrie (n + 1) a -> Litrie (n + 1) a)
push f t
  = case t
      of Cut -> Cut
         Step (Link a t')
           -> Step (Link a (f t'))
         Fork (Link a left) (Link b right)
           -> Fork (Link a (f left)) (Link b (f right))
