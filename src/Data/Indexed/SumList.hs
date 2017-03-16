{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE DataKinds
           , FlexibleContexts
           , GADTs
           , RankNTypes
           , ScopedTypeVariables
           , TypeApplications
           , TypeInType
           , TypeOperators
  #-}

module Data.Indexed.SumList
  ( SumList (..)
  , toListWith
  , fromSomeList
  )
where

import Data.Constraint ( (\\) )
import Data.Constraint.Forall ( ForallF
                              , instF
                              )

import Data.Monoid ( (<>) )

import GHC.TypeLits ( type (+)
                    , Nat
                    )


import Data.Indexed.ForallIndex ( ForallIndex
                                , instAnyIndex
                                )

import Data.Indexed.Some

import Data.Indexed.Util.ShowsListLike ( showsListLike )


data SumList f sum a
  where EmptySum :: SumList f 0 a
        (:+) :: f n a -> SumList f m a -> SumList f (n + m) a
infixr 5 :+


instance ForallIndex Show f a => Show (SumList f sum a)
  where showsPrec p
          = showsListLike p ":+" consPrec "EmptySum"
              . toListWith (showsPrecAnyIndex $ consPrec + 1)
          where consPrec = 5


showsPrecAnyIndex :: forall f n a. ForallIndex Show f a => Int -> f n a -> ShowS
showsPrecAnyIndex p = showsPrec p \\ instAnyIndex @ Show @ f @ n @ a


instance ForallF Functor f => Functor (SumList f sum)
  where fmap _ EmptySum = EmptySum
        fmap f ((x :: f n a) :+ xs)
          = fmap f x :+ fmap f xs
              \\ instF @ Functor @ f @ n


instance ForallF Foldable f => Foldable (SumList f sum)
  where foldMap _ EmptySum = mempty
        foldMap f ((x :: f n a) :+ xs)
          = foldMap f x <> foldMap f xs
              \\ instF @ Foldable @ f @ n


instance ( ForallF Functor f
         , ForallF Foldable f
         , ForallF Traversable f
         ) => Traversable (SumList f sum)
  where traverse _ EmptySum = pure EmptySum
        traverse f ((x :: f n a) :+ xs)
          = (:+) <$> traverse f x <*> traverse f xs
              \\ instF @ Traversable @ f @ n


toListWith :: (forall (n :: Nat). f n a -> r) -> SumList f sum a -> [r]
toListWith _ EmptySum = []
toListWith f (x :+ xs) = f x : toListWith f xs


fromSomeList :: [Some f a] -> Some (SumList f) a
fromSomeList [] = Some EmptySum
fromSomeList (Some x : xs)
  = forSome (Some . (x :+)) $ fromSomeList xs
