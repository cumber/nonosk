{-# LANGUAGE DataKinds
           , DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , FlexibleInstances
           , KindSignatures
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TypeApplications
           , TypeFamilies
  #-}

module Data.Indexed.Vector2
  ( Vector2 (..)
  , toLists, fromLists
  , replicate, replicate'
  , transpose
  )
where

import Prelude hiding (replicate)

import Data.Constraint ( Dict (Dict)
                       , (:-) (Sub)
                       )

import Data.Foldable ( Foldable (toList) )

import Data.Indexed.ForAnyKnownIndex ( ForAnyKnownIndex2 (instAnyKnownIndex2)
                                     , ForAnyKnownIndex2F (instAnyKnownIndex2F)
                                     )

import Data.Indexed.Index ( Index (Index) )

import Data.Indexed.Nat ( Nat, KnownNat )

import Data.Indexed.Some ( Some2 (Some2)
                         , forSome, withSome
                         )
import Data.Indexed.Vector ( Vector (Nil) )
import qualified Data.Indexed.Vector as Vector


newtype Vector2 (r :: Nat) (c :: Nat) (a :: *)
  = Vector2 { toVectors :: Vector r (Vector c a) }
  deriving (Eq, Show, Functor, Foldable, Traversable)


instance Show a => ForAnyKnownIndex2 Show Vector2 a
  where instAnyKnownIndex2 = Sub Dict

instance ForAnyKnownIndex2F Functor Vector2
  where instAnyKnownIndex2F = Sub Dict

instance ForAnyKnownIndex2F Foldable Vector2
  where instAnyKnownIndex2F = Sub Dict

instance ForAnyKnownIndex2F Traversable Vector2
  where instAnyKnownIndex2F = Sub Dict


toLists :: Vector2 r c a -> [[a]]
toLists = toList . fmap toList . toVectors


fromLists :: [[a]] -> Maybe (Some2 Vector2 a)
fromLists [] = Just . Some2 . Vector2 @ 0 @ 0 $ Nil
fromLists (row : rows)
  = withSome (Vector.fromList row) (rowsSameLength rows)
  where rowsSameLength :: forall n a
                        . KnownNat n
                       => [[a]] -> Vector n a -> Maybe (Some2 Vector2 a)
        rowsSameLength xss initial
          = let maybeRest = sequenceA $ Vector.fromListIndexed' @ n <$> xss
                maybeRows = Vector.fromList . (initial :) <$> maybeRest
             in forSome (Some2 . Vector2) <$> maybeRows


replicate :: Index r () -> Index c () -> a -> Vector2 r c a
replicate Index Index = replicate'


replicate' :: (KnownNat r, KnownNat c) => a -> Vector2 r c a
replicate' = Vector2 . Vector.replicate' . Vector.replicate'


transpose :: KnownNat c => Vector2 r c a -> Vector2 c r a
transpose = Vector2 . Vector.transpose . toVectors
