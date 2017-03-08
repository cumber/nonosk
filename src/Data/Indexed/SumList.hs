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

import Data.Indexed.Util.ShowsList ( showsList )


data SumList f sum a
  where Nil :: SumList f 0 a
        (:+) :: f n a -> SumList f m a -> SumList f (n + m) a
infixr 5 :+


instance ForallIndex Show f a => Show (SumList f sum a)
  where showsPrec _
          = showsList "[sumList|" "|]" "," . toListWith showsAnyIndex


showsAnyIndex :: forall f n a. ForallIndex Show f a => f n a -> ShowS
showsAnyIndex = shows \\ instAnyIndex @ Show @ f @ n @ a


instance ForallF Functor f => Functor (SumList f sum)
  where fmap f ((x :: f n a) :+ xs)
          = fmap f x :+ fmap f xs
              \\ instF @ Functor @ f @ n


instance ForallF Foldable f => Foldable (SumList f sum)
  where foldMap f ((x :: f n a) :+ xs)
          = foldMap f x <> foldMap f xs
              \\ instF @ Foldable @ f @ n


instance ( ForallF Functor f
         , ForallF Foldable f
         , ForallF Traversable f
         ) => Traversable (SumList f sum)
  where traverse f ((x :: f n a) :+ xs)
          = (:+) <$> traverse f x <*> traverse f xs
              \\ instF @ Traversable @ f @ n


toListWith :: (forall (n :: Nat). f n a -> r) -> SumList f sum a -> [r]
toListWith _ Nil = []
toListWith f (x :+ xs) = f x : toListWith f xs
