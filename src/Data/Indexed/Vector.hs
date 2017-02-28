{-# LANGUAGE GADTs
           , DataKinds
           , DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , FlexibleInstances
           , KindSignatures
           , PatternSynonyms
           , RankNTypes
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeApplications
           , TypeFamilies
           , TypeInType
           , TypeOperators
  #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin TypeNatSolver #-}

module Data.Indexed.Vector
  ( Vector (Nil, (:^), Cons)
  , IsZero
  , append
  , isZero
  , length
  , splitAt
  , splitAt'
  , switchZero
  , replicate
  , replicate'
  )
where

import Prelude hiding ( length
                      , replicate
                      , splitAt
                      , zipWith
                      )

import Data.Constraint
import Data.Constraint.Nat

import Data.Foldable ( toList )

import Data.Semigroup ( Semigroup ((<>)) )

import Data.Singletons.Decide ( Decision (Proved, Disproved)
                              , (:~:) (Refl)
                              , (%~)
                              )
import Data.Singletons.TypeLits ( KnownNat
                                , Nat
                                , Sing (SNat)
                                , SNat
                                )

import qualified GHC.Exts as Exts

import GHC.TypeLits ( type (+)
                    , type (-)
                    , type (<=)
                    )


import Data.Indexed.Some


data Vector (n :: Nat) a
  where Nil :: Vector 0 a
        (:^)  :: a -> Vector n a -> Vector (n + 1) a
infixr 5 :^

pattern Cons :: a -> Vector n a -> Vector (n + 1) a
pattern Cons x xs = x :^ xs
infixr 5 `Cons`

deriving instance Eq a => Eq (Vector n a)
deriving instance Ord a => Ord (Vector n a)
deriving instance Functor (Vector n)
deriving instance Foldable (Vector n)
deriving instance Traversable (Vector n)

deriving instance Functor (Some Vector)
deriving instance Foldable (Some Vector)
deriving instance Traversable (Some Vector)

instance Show a => Show (Vector n a)
  where show v = (  "[vector|"
                 <> (init . tail . show . toList) v
                 <> "|]"
                 )

(.:) = (.) . (.)
infixr 9 .:


instance Show a => Show (Some Vector a)
  where showsPrec d = (  showParen (d > 10)
                      .  showString "fromList "
                      .: shows . toList
                      )


instance Semigroup (Some Vector a)
  where (<>) = liftPlus append

instance Monoid (Some Vector a)
  where mempty = Some Nil
        mappend = (<>)


instance KnownNat n => Applicative (Vector n)
  where pure = replicate'
        (<*>) = zipWith ($)


fromList :: [a] -> Some Vector a
fromList [] = Some Nil
fromList (x:xs) = case fromList xs
                    of Some v -> Some (x `Cons` v)


instance Exts.IsList (Some Vector a)
  where type Item (Some Vector a) = a
        fromList = fromList
        toList = toList


length :: KnownNat n => Vector n a -> SNat n
length _ = SNat


append :: Vector n a -> Vector m a -> Vector (n + m) a
append Nil ys = ys
append (x :^ xs) ys = x :^ append xs ys


zipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWith _ Nil _ = Nil
zipWith f (x :^ xs) (y :^ ys) = f x y :^ zipWith f xs ys


replicate' :: forall n a. KnownNat n => a -> Vector n a
replicate' x
  = switchZero (SNat @ n)
      Nil
      (x :^ replicate' @ (n - 1) x)


replicate :: SNat n -> a -> Vector n a
replicate SNat = replicate'


splitAt' :: forall s n a. (KnownNat s, s <= n)
         => Vector n a -> (Vector s a, Vector (n - s) a)
splitAt' xs
  = switchZero (SNat @ s)
      (Nil, xs)
      ( case xs
          of x :^ xs' -> case splitAt' @ (s - 1) xs'
                           of (hs, ts) -> (x :^ hs, ts)
             _ -> error "Stupid case here to shut up the exhaustiveness \
                        \checker, which tells me that Nil wasn't matched \
                        \but then also tells me the Nil can't be matched"
      )

splitAt :: forall s n a. (s <= n)
        => SNat s -> Vector n a -> (Vector s a, Vector (n - s) a)
splitAt SNat = splitAt'

data IsZero n
  where Zero :: (0 ~ n) => IsZero n
        NonZero :: (1 <= n) => IsZero n

deriving instance Show (IsZero n)


switchZero :: forall n r. SNat n -> ((0 ~ n) => r) -> ((1 <= n) => r) -> r
switchZero n zero nonzero
  = case n %~ SNat @ 0
      of Proved Refl -> zero
         Disproved _ -> nonzero \\ plusMonotone1 @ 0 @ (n - 1) @ 1


isZero :: SNat n -> IsZero n
isZero n = switchZero n Zero NonZero
