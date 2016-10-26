{-# LANGUAGE DataKinds
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , KindSignatures
           , MultiParamTypeClasses
           , PolyKinds
           , RankNTypes
           , ScopedTypeVariables
           , StandaloneDeriving
           , TemplateHaskell
  #-}

module Skapix.Puzzle
where

import Control.Lens (makeFields)

import Control.Monad (guard)

import Data.Type.Natural (Nat(S, Z), SNat, intToNat)

import Data.Singletons (SomeSing(SomeSing), toSing)

import Data.Vector.Sized (Vector((:-), Nil))
import qualified Data.Vector.Sized as V

import Numeric.Natural (Natural)


data Hint a = Hint { hintRun :: Natural, hintValue :: a }
  deriving (Eq, Show)

$(makeFields ''Hint)


type Cell a = Maybe a


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


inferLine :: Eq a => [Hint a] -> Vector a n -> [Vector a n]
inferLine [] = guard


transform
 :: (Vector a Z -> Vector b Z)
 -> (forall n. (Vector a n -> Vector b n) -> (Vector a (S n) -> Vector b (S n)))
 -> (Vector a m -> Vector b m)
transform base _induct Nil = base Nil
transform base induct v@(_ :- _) = induct (transform base induct) v
