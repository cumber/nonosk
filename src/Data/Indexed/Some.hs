{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE DataKinds
           , GADTs
           , KindSignatures
           , FlexibleContexts
           , RankNTypes
           , ScopedTypeVariables
           , TypeApplications
           , TypeOperators
           , UndecidableInstances
  #-}

module Data.Indexed.Some
  ( Some (..)
  , forSome
  , withSome
  , guessIndex
  , guessIndex'
  , liftPlus

  , someIndex

  , Some2 (..)
  , forSome2
  , withSome2
  , guessIndex2
  , guessIndex2'
  )
where

import Data.Constraint ( (:-)
                       , (\\)
                       )

import Data.Kind ( Type )

import Data.Proxy ( Proxy (Proxy) )

import Data.Type.Equality ( (:~:) (Refl) )

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
                                     , ForAnyKnownIndex2F (instAnyKnownIndex2F)
                                     )

import Data.Indexed.Index ( Index (Index)
                          , withIndexOf
                          , sameIndex'
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
  where Some (xs :: f n a) == ys
          = Just xs == guessIndex' @n ys
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


guessIndex :: Index n () -> Some f a -> Maybe (f n a)
guessIndex n (Some x) = withIndexOf n id x

guessIndex' :: forall n f a. KnownNat n => Some f a -> Maybe (f n a)
guessIndex' = guessIndex Index


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


instance ForAnyKnownIndex2 Eq f a => Eq (Some2 f (a :: Type))
  where Some2 (xs :: f n m a) == ys
          = Just xs == guessIndex2' @n @m ys
              \\ ( instAnyKnownIndex2
                     :: (KnownNat n, KnownNat m) :- Eq (f n m a)
                 )


instance ForAnyKnownIndex2F Functor f => Functor (Some2 f)
  where fmap f (Some2 (x :: f n m a))
          = Some2 (fmap f x)
              \\ ( instAnyKnownIndex2F
                     :: (KnownNat n, KnownNat m) :- Functor (f n m)
                 )


instance ForAnyKnownIndex2F Foldable f => Foldable (Some2 f)
  where foldMap f (Some2 (x :: f n m a))
          = foldMap f x
              \\ ( instAnyKnownIndex2F
                     :: (KnownNat n, KnownNat m) :- Foldable (f n m)
                 )


instance ( ForAnyKnownIndex2F Functor f
         , ForAnyKnownIndex2F Foldable f
         , ForAnyKnownIndex2F Traversable f
         ) => Traversable (Some2 f)
  where traverse f (Some2 (x :: f n m a))
          = Some2 <$> traverse f x
              \\ ( instAnyKnownIndex2F
                     :: (KnownNat n, KnownNat m) :- Traversable (f n m)
                 )

forSome2 :: (forall n m. (KnownNat n, KnownNat m) => f n m a -> r)
          -> (Some2 f a -> r)
forSome2 f (Some2 x) = f x

withSome2 :: Some2 f a
          -> (forall n m. (KnownNat n, KnownNat m) => f n m a -> r)
          -> r
withSome2 s f = forSome2 f s


guessIndex2 :: forall n m f a
              . Index n () -> Index m () -> Some2 f a -> Maybe (f n m a)
guessIndex2 Index Index = guessIndex2'

guessIndex2' :: forall n m f a
              . (KnownNat n, KnownNat m)
             => Some2 f a -> Maybe (f n m a)
guessIndex2' (Some2 (x :: f n' m' a))
  = do Refl <- sameIndex' @ n @ n'
       Refl <- sameIndex' @ m @ m'
       pure x
