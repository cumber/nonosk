{-# LANGUAGE GADTs
           , DataKinds
           , KindSignatures
           , RankNTypes
           , TypeFamilyDependencies
           , TypeOperators
           , TypeInType
  #-}

module Data.Indexed.Some
  ( Some (..)
  , withSome
  , liftPlus
  )
where

import GHC.TypeLits (type (+))

import Data.Singletons.TypeLits (Nat)


data Some f a
  where Some :: f (n :: Nat) a -> Some f a


withSome :: (forall n. f n a -> r) -> Some f a -> r
withSome f (Some x) = f x

liftPlus :: (forall n m. f n a -> g m b -> h (n + m) c)
         -> (Some f a -> Some g b -> Some h c)
liftPlus f (Some x) (Some y) = Some (f x y)
