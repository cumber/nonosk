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

import Data.Vector.Sized (Vector(Nil, (:-)))
import qualified Data.Vector.Sized as V

import Data.Type.Natural

import Numeric.Natural


newtype Grid (r :: Nat) (c :: Nat) (a :: *)
  = Grid { unGrid :: Vector (Vector a c) r }
  deriving (Eq, Show)

data Puzzle :: * -> *
  where Puzzle     :: { unPuzzle :: Grid (r :: Nat) (c :: Nat) (a :: *)
                      } -> Puzzle a

deriving instance Show a => Show (Puzzle a)


toLists :: Grid r c a -> [[a]]
toLists = V.toList . V.map V.toList . unGrid


instance Eq a => Eq (Puzzle a)
  where Puzzle g == Puzzle h = toLists g == toLists h


instance Functor Puzzle
  where fmap f (Puzzle (Grid v)) = Puzzle . Grid . V.map (V.map f) $ v


data Some :: (k -> *) -> *
  where Some :: f x -> Some f


smap :: (forall a. f a -> g (q a)) -> Some f -> Some g
smap f (Some x) = Some (f x)

sbind :: (forall a. f a -> Some g) -> Some f -> Some g
sbind f (Some x) = f x

elim :: (forall a. f a -> c) -> Some f -> c
elim f (Some x) = f x

what :: (forall r a. f a -> (forall b. g b -> r g) -> r g) -> Some f -> Some g
what f (Some x) = f x Some

cps :: (f a -> g b) -> f a -> (forall c. g c -> r) -> r
cps f a c = c (f a)



fromListSome :: [a] -> Some (Vector a)
fromListSome = foldr (\x -> smap (x :-)) (Some Nil)


initPuzzle :: a -> [Natural] -> [Natural] -> Puzzle a
initPuzzle cell rowHints colHints
  = Puzzle (Grid Nil)
