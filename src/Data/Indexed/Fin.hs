{-# LANGUAGE AllowAmbiguousTypes
           , DataKinds
           , FlexibleInstances
           , GADTs
           , KindSignatures
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TypeApplications
           , TypeOperators
  #-}
module Data.Indexed.Fin
  ( Fin
  , fromFin
  , toFin, toFin'
  , inBoundsToFin, inBoundsToFin'
  , unsafeToFin, unsafeToFin'

  , down
  , up

  , Ordinal
  , toOrdinal, toOrdinal'
  , toNatural
  )
where

import Data.Coerce ( coerce )

import Data.Constraint ( Dict (Dict)
                       , (:-) (Sub)
                       )

import Numeric.Natural ( Natural )


import Data.Indexed.ForAnyKnownIndex ( ForAnyKnownIndex (instAnyKnownIndex) )

import Data.Indexed.Index ( Index (Index)
                          , index'
                          )

import Data.Indexed.Nat ( Nat
                        , KnownNat
                        , type (>=)
                        , type (+), type (-)
                        )


newtype Fin (n :: Nat) a
  = Fin { fromFin :: a }
  deriving (Eq, Ord)


instance Show a => Show (Fin n a)
  where showsPrec p (Fin x)
          = showParen (p > appPrec)
              ( showString "Fin "
              . showsPrec (appPrec + 1) x
              )
          where appPrec = 10

instance (KnownNat n, n >= 1, Integral a) => Bounded (Fin n a)
  where minBound = Fin 0
        maxBound = Fin . fromIntegral $ index' @ n - 1

instance (KnownNat n, Integral a) => Enum (Fin n a)
  where succ (Fin x)
          | fromIntegral x >= index' @ n - 1
          = error "Tried to take succ of maxBound"
          | otherwise
          = Fin $ succ x

        pred (Fin x)
          | x <= 0     = error "Tried to take pred of minBound"
          | otherwise  = Fin (x - 1)

        fromEnum = coerce (fromEnum @ a)

        toEnum i
         | inBounds @ n i = Fin (toEnum i)
         | otherwise      = error "toEnum out of range"

        enumFrom = coerce (takeWhile (inBounds @ n) . enumFrom @ a)

        enumFromTo = coerce (enumFromTo @ a)

        enumFromThen
          = coerce (takeWhile (inBounds @ n) .: enumFromThen @ a)

        enumFromThenTo = coerce (enumFromThenTo @ a)


{-|
Similar to 'pred' from 'Enum', with the differences:

  * Returns 'Nothing' rather than calling error when going out of range
  * If the result is 'Just', the resulting 'Fin' has an index one smaller than
    the input

This is much more useful when writing recursive functions using a 'Fin' to
indicate a position in an inductive structure; case analysis on the 'Maybe'
tells you whether you've reached the right position ('Nothing'), and if not
gives you a 'Fin' of the right type to pass to the recursive call.
-}
down :: (Integral i, n >= 1) => Fin n i -> Maybe (Fin (n - 1) i)
down (Fin x)
  | x > 0      = Just $ Fin (x - 1)
  | otherwise  = Nothing

{-|
Similar to 'succ' from 'Enum', but also increases the index of the 'Fin' (it's
therefore impossible to go out of range when using an unbounded representation
type. Therefore the result is given directly, not in a 'Maybe' as is the case
with 'down'.
-}
up :: Integral i => Fin n i -> Fin (n + 1) i
up (Fin x) = Fin (x + 1)

(.:) = (.) . (.)

inBounds :: forall n a. (KnownNat n, Integral a) => a -> Bool
inBounds i = i >= 0 && fromIntegral i < index' @ n


instance Eq a => ForAnyKnownIndex Eq Fin a
  where instAnyKnownIndex = Sub Dict

instance Ord a => ForAnyKnownIndex Ord Fin a
  where instAnyKnownIndex = Sub Dict

instance Show a => ForAnyKnownIndex Show Fin a
  where instAnyKnownIndex = Sub Dict

instance Integral a => ForAnyKnownIndex Enum Fin a
  where instAnyKnownIndex = Sub Dict


toFin :: Integral a => Index n () -> a -> Maybe (Fin n a)
toFin Index = toFin'

toFin' :: forall n a. (KnownNat n, Integral a) => a -> Maybe (Fin n a)
toFin' = toFinImpl @ n Nothing (Just . Fin)

inBoundsToFin :: forall n a. Integral a => Index n () -> a -> Fin n a
inBoundsToFin Index = inBoundsToFin'

inBoundsToFin' :: forall n a. (KnownNat n, Integral a) => a -> Fin n a
inBoundsToFin' = toFinImpl @ n (error "Fin out of bounds") Fin

toFinImpl :: forall n a r. (KnownNat n, Integral a) => r -> (a -> r) -> a -> r
toFinImpl errorCase successCase x
  | inBounds @ n x  = successCase x
  | otherwise       = errorCase

unsafeToFin :: Integral a => Index n () -> a -> Fin n a
unsafeToFin Index = unsafeToFin'

unsafeToFin' :: forall n a. (KnownNat n, Integral a) => a -> Fin n a
unsafeToFin' = Fin

type Ordinal n = Fin n Natural


toOrdinal :: Index n () -> Natural -> Maybe (Ordinal n)
toOrdinal Index = toOrdinal'

toOrdinal' :: forall n. KnownNat n => Natural -> Maybe (Ordinal n)
toOrdinal' x
  | x < index' @ n  = Just (Fin x)
  | otherwise       = Nothing


toNatural :: Ordinal n -> Natural
toNatural = fromFin
