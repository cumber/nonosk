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
  , forSome
  , withSome
  , liftPlus

  , Some2 (..)
  , forSome2
  , withSome2
  )
where

import GHC.TypeLits ( Nat
                    , KnownNat
                    , type (+)
                    )

import Data.Constraint ( (\\) )
import Data.Constraint.Forall ( ForallF
                              , instF
                              )

import Data.Indexed.ForallIndex ( ForallIndex
                                , instAnyIndex
                                , ForallIndex2
                                , instAnyIndex2
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

-- | Handle a @Some f a@ with a function that can handle @f n a@ for all @n@
withSome :: Some f a -> (forall n. KnownNat n => f n a -> r) -> r
withSome s f = forSome f s

-- | Lift a function of @f n a@ for all @n@ to a function of @Some f a@
forSome :: (forall n. KnownNat n => f n a -> r) -> (Some f a -> r)
forSome f (Some x) = f x


liftPlus :: (forall n m. f n a -> g m b -> h (n + m) c)
         -> (Some f a -> Some g b -> Some h c)
liftPlus f (Some x) (Some y) = Some (f x y)



data Some2 f a
  where Some2 :: (KnownNat n, KnownNat m)
              => f (n :: Nat) (m :: Nat) a -> Some2 f a


instance ForallIndex2 Show f a => Show (Some2 f a)
  where showsPrec p (Some2 (x :: f n m a))
          = showParen (p > appPrec)
              ( showString "Some2 "
              . showsPrec (appPrec + 1) x
              )
              \\ instAnyIndex2 @ Show @ f @ n @ m @ a
          where appPrec = 10


withSome2 :: Some2 f a
          -> (forall n m. (KnownNat n, KnownNat m) => f n m a -> r)
          -> r
withSome2 s f = forSome2 f s

forSome2 :: (forall n m. (KnownNat n, KnownNat m) => f n m a -> r)
          -> (Some2 f a -> r)
forSome2 f (Some2 x) = f x
