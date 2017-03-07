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
           , ViewPatterns
  #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin TypeNatSolver #-}

module Data.Indexed.Vector
  ( Vector (Nil, (:^))
  , append
  , length
  , splitAt
  , splitAt'
  , replicate
  , replicate'
  )
where

import Prelude hiding ( length
                      , replicate
                      , splitAt
                      , zipWith
                      )

import Data.Foldable ( toList )

import Data.Semigroup ( Semigroup ((<>)) )

import qualified GHC.Exts as Exts

import GHC.TypeLits ( type (+)
                    , type (-)
                    , type (<=)
                    , KnownNat
                    )


import Data.Indexed.Index
import Data.Indexed.Some


data Vector n a
  where Nil  :: Vector 0 a
        (:^) :: a -> Vector n a -> Vector (n + 1) a
infixr 5 :^


deriving instance Eq a => Eq (Vector n a)
deriving instance Ord a => Ord (Vector n a)
deriving instance Functor (Vector n)
deriving instance Foldable (Vector n)
deriving instance Traversable (Vector n)

instance Show a => Show (Vector n a)
  where show v = (  "[vector|"
                 <> (init . tail . show . toList) v
                 <> "|]"
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
                    of Some v -> Some (x :^ v)


instance Exts.IsList (Some Vector a)
  where type Item (Some Vector a) = a
        fromList = fromList
        toList = toList


length :: KnownNat n => Vector n a -> Index n
length _ = Index


append :: Vector n a -> Vector m a -> Vector (n + m) a
append Nil ys = ys
append (x :^ xs) ys = x :^ append xs ys


zipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWith _ Nil _ = Nil
zipWith f (x :^ xs) (y :^ ys) = f x y :^ zipWith f xs ys


replicate' :: forall n a. KnownNat n => a -> Vector n a
replicate' x
  = switchZero (Index @ n)
      Nil
      (x :^ replicate' @ (n - 1) x)


replicate :: Index n -> a -> Vector n a
replicate Index = replicate'


splitAt' :: forall s n a. (KnownNat s, s <= n)
         => Vector n a -> (Vector s a, Vector (n - s) a)
splitAt' xs
  = switchZero (Index @ s)
      (Nil, xs)
      ( case xs
          of x :^ xs' -> case splitAt' @ (s - 1) xs'
                           of (hs, ts) -> (x :^ hs, ts)
             _ -> error "Stupid case here to shut up the exhaustiveness \
                        \checker, which tells me that Nil wasn't matched \
                        \but then also tells me the Nil can't be matched"
      )

splitAt :: forall s n a. (s <= n)
        => Index s -> Vector n a -> (Vector s a, Vector (n - s) a)
splitAt Index = splitAt'
