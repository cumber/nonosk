{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE GADTs
           , FlexibleContexts
           , RankNTypes
           , ScopedTypeVariables
           , TypeApplications
           , TypeOperators
           , TypeInType
  #-}

module Data.Indexed.Some
  ( Some (..)
  , withSome
  , liftPlus
  )
where

import GHC.TypeLits ( KnownNat
                    , type (+)
                    )

import Data.Constraint ( (\\) )
import Data.Constraint.Forall ( ForallF
                              , instF
                              )

import Data.Singletons.TypeLits ( Nat )

import Data.Indexed.ForallIndex ( ForallIndex
                                , instAnyIndex
                                )


data Some f a
  where Some :: KnownNat n => f (n :: Nat) a -> Some f a


instance ForallIndex Show f a => Show (Some f a)
  where showsPrec p (Some (x :: f n a))
          = showParen (p > appPrec)
              ( showString "Some "
              . showsPrec (appPrec + 1) x
              )
              \\ instAnyIndex @ Show @ f @ n @ a
          where appPrec = 10


instance ForallF Functor f => Functor (Some f)
  where fmap f (Some (x :: f n a))
          = Some (fmap f x)
              \\ instF @ Functor @ f @ n


instance ForallF Foldable f => Foldable (Some f)
  where foldMap f (Some (x :: f n a))
          = foldMap f x
              \\ instF @ Foldable @ f @ n


instance ( ForallF Functor f
         , ForallF Foldable f
         , ForallF Traversable f
         ) => Traversable (Some f)
  where traverse f (Some (x :: f n a))
          = Some <$> traverse f x
              \\ instF @ Traversable @ f @ n


withSome :: (forall n. f n a -> r) -> (Some f a -> r)
withSome f (Some x) = f x

liftPlus :: (forall n m. f n a -> g m b -> h (n + m) c)
         -> (Some f a -> Some g b -> Some h c)
liftPlus f (Some x) (Some y) = Some (f x y)
