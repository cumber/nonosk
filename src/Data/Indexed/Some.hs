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

  , someIndex

  , Some2 (..)
  , forSome2
  , withSome2
  )
where

import Data.Constraint ( (:-)
                       , (\\)
                       )

import Data.Kind ( Type )

import Data.Maybe ( fromMaybe )

import Data.Proxy ( Proxy (Proxy) )

import Data.Semigroup ( (<>) )

import GHC.TypeLits ( SomeNat (SomeNat)
                    , someNatVal
                    )

import Numeric.Natural ( Natural )


import Data.Indexed.Nat ( Nat, KnownNat
                        , type (+)
                        )

import Data.Indexed.ForAnyKnownIndex ( ForAnyKnownIndex (instAnyKnownIndex)
                                     , ForAnyKnownIndexF (instAnyKnownIndexF)
                                     , ForAnyKnownIndex2 (instAnyKnownIndex2)
                                     )

import Data.Indexed.Index ( Index (Index)
                          , withSameIndex
                          )


data Some f a
  where Some :: KnownNat n => f (n :: Nat) a -> Some f a


instance ForAnyKnownIndex Show f a => Show (Some f a)
  where showsPrec p (Some (x :: f n a))
          = showParen (p > appPrec)
              ( showString "Some "
              . showsPrec (appPrec + 1) x
              )
              \\ (instAnyKnownIndex :: KnownNat n :- Show (f n a))
          where appPrec = 10


instance ForAnyKnownIndex Eq f a => Eq (Some f (a :: Type))
  where Some (xs :: f n a) == Some ys
          = fromMaybe False $ withSameIndex (==) xs ys
                  \\ (instAnyKnownIndex :: KnownNat n :- Eq (f n a))


instance ForAnyKnownIndexF Functor f => Functor (Some f)
  where fmap f (Some (x :: f n a))
          = Some (fmap f x)
              \\ (instAnyKnownIndexF :: KnownNat n :- Functor (f n))


instance ForAnyKnownIndexF Foldable f => Foldable (Some f)
  where foldMap f (Some (x :: f n a))
          = foldMap f x
              \\ (instAnyKnownIndexF :: KnownNat n :- Foldable (f n))


instance ( ForAnyKnownIndexF Functor f
         , ForAnyKnownIndexF Foldable f
         , ForAnyKnownIndexF Traversable f
         ) => Traversable (Some f)
  where traverse f (Some (x :: f n a))
          = Some <$> traverse f x
              \\ (instAnyKnownIndexF :: KnownNat n :- Traversable (f n))


-- | Lift a function of @f n a@ for all @n@ to a function of @Some f a@
forSome :: (forall n. KnownNat n => f n a -> r) -> (Some f a -> r)
forSome f (Some x) = f x

-- | Handle a @Some f a@ with a function that can handle @f n a@ for all @n@
withSome :: Some f a -> (forall n. KnownNat n => f n a -> r) -> r
withSome s f = forSome f s


liftPlus :: (forall n m. f n a -> g m b -> h (n + m) c)
         -> (Some f a -> Some g b -> Some h c)
liftPlus f (Some x) (Some y) = Some (f x y)


someIndex :: Natural -> Some Index ()
someIndex x
  = case someNatVal . fromIntegral $ x
       of Just (SomeNat (Proxy :: Proxy n)) -> Some (Index @ n)
          Nothing -> error $ "Impossible: Natural " <> show x
                               <> " with no corresponding Nat"


data Some2 f a
  where Some2 :: (KnownNat n, KnownNat m)
              => f (n :: Nat) (m :: Nat) a -> Some2 f a


instance ForAnyKnownIndex2 Show f a => Show (Some2 f a)
  where showsPrec p (Some2 (x :: f n m a))
          = showParen (p > appPrec)
              ( showString "Some2 "
              . showsPrec (appPrec + 1) x
              )
              \\ (  instAnyKnownIndex2
                 :: (KnownNat n, KnownNat m) :- Show (f n m a)
                 )
          where appPrec = 10


forSome2 :: (forall n m. (KnownNat n, KnownNat m) => f n m a -> r)
          -> (Some2 f a -> r)
forSome2 f (Some2 x) = f x

withSome2 :: Some2 f a
          -> (forall n m. (KnownNat n, KnownNat m) => f n m a -> r)
          -> r
withSome2 s f = forSome2 f s
