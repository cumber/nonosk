{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE DataKinds
           , GeneralizedNewtypeDeriving
           , KindSignatures
           , ScopedTypeVariables
           , TypeApplications
           , TypeFamilies
           , TypeOperators
  #-}

module Nonosk.PosSet
  ( Pos, PosSet

  , null
  , size
  , member

  , empty
  , allPositions
  , singleton
  , insert
  , delete

  , union
  , intersection
  , difference

  , foldr

  , fromList
  , fromFoldable
  , toList

  , flattenPos
  )
where

import Prelude hiding ( null
                      , foldr
                      )

import qualified Control.Lens as Lens

import Data.Bifunctor ( bimap )

import Data.Coerce ( coerce )

import qualified Data.Foldable as Foldable

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.Semigroup ( Semigroup )

import Data.Indexed.Index ( index' )
import Data.Indexed.Fin ( Fin
                        , fromFin
                        , unsafeToFin'
                        )

import Data.Indexed.Nat ( Nat
                        , KnownNat
                        , type (×)
                        )


type Pos r c = (Fin r Int, Fin c Int)

flattenPos :: forall r c
            . (KnownNat r, KnownNat c)
           => Lens.Iso' (Pos r c) (Fin (r × c) Int)
flattenPos = Lens.iso posToFin finToPos
  where width = fromIntegral $ index' @ c
        finToPos = bimap unsafeToFin' unsafeToFin' . flip quotRem width . fromFin
        posToFin (x, y) = unsafeToFin' $ fromFin x * width + fromFin y


newtype PosSet (r :: Nat) (c :: Nat)
  = PosSet { getIntSet :: IntSet }
  deriving (Eq, Ord, Semigroup, Monoid)


instance (KnownNat r, KnownNat c) => Show (PosSet r c)
  where showsPrec p xs
          = showParen (p > appPrec)
              ( showString "fromList "
              . shows (toList xs)
              )
          where appPrec = 10


null :: PosSet r c -> Bool
null = coerce IntSet.null

size :: PosSet r c -> Int
size = coerce IntSet.size

member :: (KnownNat r, KnownNat c) => Pos r c -> PosSet r c -> Bool
member = (result . argument) getIntSet ( IntSet.member
                                       . fromFin
                                       . Lens.view flattenPos
                                       )


empty :: PosSet r c
empty = coerce IntSet.empty

allPositions :: forall r c. (KnownNat r, KnownNat c) => PosSet r c
allPositions
  = let rows = fromIntegral $ index' @ r
        cols = fromIntegral $ index' @ c
     in PosSet $ IntSet.fromList [0 .. rows * cols - 1]

singleton :: (KnownNat r, KnownNat c) => Pos r c -> PosSet r c
singleton = flip insert empty

insert :: (KnownNat r, KnownNat c) => Pos r c -> PosSet r c -> PosSet r c
insert = coerce IntSet.insert . fromFin . Lens.view flattenPos

delete :: (KnownNat r, KnownNat c) => Pos r c -> PosSet r c -> PosSet r c
delete = coerce IntSet.delete . fromFin . Lens.view flattenPos


union :: PosSet r c -> PosSet r c -> PosSet r c
union = coerce IntSet.union

intersection :: PosSet r c -> PosSet r c -> PosSet r c
intersection = coerce IntSet.intersection

difference :: PosSet r c -> PosSet r c -> PosSet r c
difference = coerce IntSet.difference


foldr :: (KnownNat r, KnownNat c) => (Pos r c -> b -> b) -> b -> PosSet r c -> b
foldr f
  = coerce $ IntSet.foldr (argument (Lens.review flattenPos . unsafeToFin') f)


fromList :: (KnownNat r, KnownNat c) => [Pos r c] -> PosSet r c
fromList = coerce IntSet.fromList . map (fromFin . Lens.view flattenPos)

fromFoldable :: (Foldable f, KnownNat r, KnownNat c) => f (Pos r c) -> PosSet r c
fromFoldable = fromList . Foldable.toList

toList :: (KnownNat r, KnownNat c) => PosSet r c -> [Pos r c]
toList = map (Lens.review flattenPos . unsafeToFin') . coerce IntSet.toList

result = (.)
argument = flip (.)
