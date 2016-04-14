{-# LANGUAGE DataKinds
           , ExplicitForAll
           , GADTs
           , KindSignatures
           , PolyKinds
           , RankNTypes
           , ScopedTypeVariables
           , StandaloneDeriving
  #-}

module Skapix.Puzzle
where

import Data.Type.Natural (Nat, SNat, intToNat)

import Data.Vector.Sized (Vector)
import qualified Data.Vector.Sized as V

import Numeric.Natural (Natural)

import Data.Singletons (SomeSing(SomeSing), toSing)


newtype Grid (r :: Nat) (c :: Nat) (a :: *)
  = Grid { unGrid :: Vector (Vector a c) r }
  deriving (Eq, Show)

data Puzzle :: * -> *
  where Puzzle     :: { unPuzzle :: Grid (r :: Nat) (c :: Nat) (a :: *)
                      } -> Puzzle a

deriving instance Show a => Show (Puzzle a)


toLists :: Grid r c a -> [[a]]
toLists = V.toList . V.map V.toList . unGrid


constGrid :: SNat r -> SNat c -> a -> Grid r c a
constGrid r c = Grid . V.replicate r . V.replicate c


instance Eq a => Eq (Puzzle a)
  where Puzzle g == Puzzle h = toLists g == toLists h


instance Functor Puzzle
  where fmap f (Puzzle (Grid v)) = Puzzle . Grid . V.map (V.map f) $ v


initPuzzle :: a -> [Natural] -> [Natural] -> Puzzle a
initPuzzle cell rowHints colHints
  = let nRows = intToNat $ length rowHints
        nCols = intToNat $ length colHints
    in  case (toSing nRows, toSing nCols) of
          (SomeSing r, SomeSing c) -> Puzzle $ constGrid r c cell
