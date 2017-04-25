{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE AllowAmbiguousTypes
           , DataKinds
           , GeneralizedNewtypeDeriving
           , KindSignatures
           , ScopedTypeVariables
           , TypeApplications
           , TypeFamilyDependencies
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

  , Direction (..)
  , Linearise (..)
  , transpose
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

import Data.Tuple ( swap )


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

rowOrder :: forall r c
          . (KnownNat r, KnownNat c)
         => Lens.Iso' (Pos r c) (Fin (r × c) Int)
rowOrder = Lens.iso posToFin finToPos
  where width = fromIntegral $ index' @ c
        finToPos = ( bimap unsafeToFin' unsafeToFin'
                   . flip quotRem width
                   . fromFin
                   )
        posToFin (x, y) = unsafeToFin' $ fromFin x * width + fromFin y


columnOrder :: forall r c
             . (KnownNat r, KnownNat c)
            => Lens.Iso' (Pos r c) (Fin (r × c) Int)
columnOrder = Lens.iso posToFin finToPos
  where height = fromIntegral $ index' @ r
        finToPos = ( bimap unsafeToFin' unsafeToFin'
                   . swap
                   . flip quotRem height
                   . fromFin
                   )
        posToFin (x, y) = unsafeToFin' $ fromFin y * height + fromFin x


data Direction = RowOrder | ColumnOrder

newtype PosSet (d :: Direction) (r :: Nat) (c :: Nat)
  = PosSet { getIntSet :: IntSet }
  deriving (Eq, Ord, Semigroup, Monoid)


instance (KnownNat r, KnownNat c) => Show (PosSet d r c)
  where showsPrec p xs
          = showParen (p > appPrec)
              ( showString "fromList "
              . shows (toList xs)
              )
          where appPrec = 10


type family Transpose d = r | r -> d
  where Transpose RowOrder = ColumnOrder
        Transpose ColumnOrder = RowOrder

class Linearise (k :: Direction)
  where linearise :: (KnownNat r, KnownNat c)
                  => Lens.Iso' (Pos r c) (Fin (r × c) Int)

instance Linearise RowOrder
  where linearise = rowOrder

instance Linearise ColumnOrder
  where linearise = columnOrder

transpose :: PosSet d r c -> PosSet (Transpose d) c r
transpose = coerce


null :: PosSet d r c -> Bool
null = coerce IntSet.null

size :: PosSet d r c -> Int
size = coerce IntSet.size

member :: (KnownNat r, KnownNat c) => Pos r c -> PosSet d r c -> Bool
member = (result . argument) getIntSet ( IntSet.member
                                       . fromFin
                                       . Lens.view (linearise @ RowOrder)
                                       )


empty :: PosSet d r c
empty = coerce IntSet.empty

allPositions :: forall d r c. (KnownNat r, KnownNat c) => PosSet d r c
allPositions
  = let rows = fromIntegral $ index' @ r
        cols = fromIntegral $ index' @ c
     in PosSet $ IntSet.fromList [0 .. rows * cols - 1]

singleton :: (KnownNat r, KnownNat c) => Pos r c -> PosSet d r c
singleton = flip insert empty

insert :: (KnownNat r, KnownNat c) => Pos r c -> PosSet d r c -> PosSet d r c
insert = coerce IntSet.insert . fromFin . Lens.view (linearise @ RowOrder)

delete :: (KnownNat r, KnownNat c) => Pos r c -> PosSet d r c -> PosSet d r c
delete = coerce IntSet.delete . fromFin . Lens.view (linearise @ RowOrder)


union :: PosSet d r c -> PosSet d r c -> PosSet d r c
union = coerce IntSet.union

intersection :: PosSet d r c -> PosSet d r c -> PosSet d r c
intersection = coerce IntSet.intersection

difference :: PosSet d r c -> PosSet d r c -> PosSet d r c
difference = coerce IntSet.difference


foldr :: (KnownNat r, KnownNat c)
      => (Pos r c -> b -> b) -> b -> PosSet d r c -> b
foldr f
  = coerce ( IntSet.foldr (argument (Lens.review (linearise @ RowOrder)
           . unsafeToFin') f)
           )


fromList :: (KnownNat r, KnownNat c) => [Pos r c] -> PosSet d r c
fromList = coerce IntSet.fromList
             . map (fromFin . Lens.view (linearise @ RowOrder))

fromFoldable :: (Foldable f, KnownNat r, KnownNat c)
             => f (Pos r c) -> PosSet d r c
fromFoldable = fromList . Foldable.toList

toList :: (KnownNat r, KnownNat c) => PosSet d r c -> [Pos r c]
toList = map (Lens.review (linearise @ RowOrder) . unsafeToFin')
           . coerce IntSet.toList

result = (.)
argument = flip (.)
