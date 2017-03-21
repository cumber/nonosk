{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , GADTs
           , MultiParamTypeClasses
           , RankNTypes
           , ScopedTypeVariables
           , TypeInType
           , TypeOperators
  #-}

module Data.Indexed.SumList
  ( SumList (..)
  , toListWith
  , fromSomeList
  )
where

import Data.Constraint ( (:-) (Sub)
                       , (\\)
                       , Dict (Dict)
                       )

import Data.Monoid ( (<>) )


import Data.Indexed.ForAnyKnownIndex ( ForAnyKnownIndexF (instAnyKnownIndexF)
                                     , ForAnyKnownIndex (instAnyKnownIndex)
                                     )

import Data.Indexed.Nat ( Nat, KnownNat
                        , type (+)
                        )

import Data.Indexed.Some ( Some (Some)
                         , forSome
                         )

import Data.Indexed.Util.ShowsListLike ( showsListLike )


data SumList f sum a
  where EmptySum :: SumList f 0 a
        (:+) :: KnownNat n => f n a -> SumList f m a -> SumList f (n + m) a
infixr 5 :+


instance ForAnyKnownIndex Show f a => Show (SumList f sum a)
  where showsPrec p
          = showsListLike p ":+" consPrec "EmptySum"
              . toListWith (showsPrecAnyIndex $ consPrec + 1)
          where consPrec = 5

instance ForAnyKnownIndex Show f a => ForAnyKnownIndex Show (SumList f) a
  where instAnyKnownIndex = Sub Dict


showsPrecAnyIndex :: forall f n a
                   . (KnownNat n, ForAnyKnownIndex Show f a)
                  => Int -> f n a -> ShowS
showsPrecAnyIndex p = showsPrec p \\ ( instAnyKnownIndex
                                     :: KnownNat n :- Show (f n a)
                                     )


instance ForAnyKnownIndexF Functor f => Functor (SumList f sum)
  where fmap _ EmptySum = EmptySum
        fmap f ((x :: f n a) :+ xs)
          = fmap f x :+ fmap f xs
              \\ ( instAnyKnownIndexF :: KnownNat n :- Functor (f n) )

instance ForAnyKnownIndexF Functor f => ForAnyKnownIndexF Functor (SumList f)
  where instAnyKnownIndexF = Sub Dict


instance ForAnyKnownIndexF Foldable f => Foldable (SumList f sum)
  where foldMap _ EmptySum = mempty
        foldMap f ((x :: f n a) :+ xs)
          = foldMap f x <> foldMap f xs
              \\ ( instAnyKnownIndexF :: KnownNat n :- Foldable (f n) )

instance ForAnyKnownIndexF Foldable f => ForAnyKnownIndexF Foldable (SumList f)
  where instAnyKnownIndexF = Sub Dict


instance ( ForAnyKnownIndexF Functor f
         , ForAnyKnownIndexF Foldable f
         , ForAnyKnownIndexF Traversable f
         ) => Traversable (SumList f sum)
  where traverse _ EmptySum = pure EmptySum
        traverse f ((x :: f n a) :+ xs)
          = (:+) <$> traverse f x <*> traverse f xs
              \\ ( instAnyKnownIndexF :: KnownNat n :- Traversable (f n) )

instance ( ForAnyKnownIndexF Functor f
         , ForAnyKnownIndexF Foldable f
         , ForAnyKnownIndexF Traversable f
         ) => ForAnyKnownIndexF Traversable (SumList f)
  where instAnyKnownIndexF = Sub Dict


toListWith :: (forall (n :: Nat). KnownNat n => f n a -> r)
           -> (SumList f sum a -> [r])
toListWith _ EmptySum = []
toListWith f (x :+ xs) = f x : toListWith f xs


fromSomeList :: [Some f a] -> Some (SumList f) a
fromSomeList [] = Some EmptySum
fromSomeList (Some x : xs)
  = forSome (Some . (x :+)) $ fromSomeList xs
