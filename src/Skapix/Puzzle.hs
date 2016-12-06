{-# LANGUAGE DataKinds
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , KindSignatures
           , MultiParamTypeClasses
           , PatternSynonyms
           , PolyKinds
           , RankNTypes
           , ScopedTypeVariables
           , StandaloneDeriving
           , TemplateHaskell
           , TypeFamilies
           , TypeOperators
  #-}

module Skapix.Puzzle
where

import Control.Lens (makeFields)

import Control.Monad (guard)

--import Data.Type.Natural (Nat(S, Z), SNat, intToNat)
import Data.Sized.Builtin (Sized, pattern (:<))
import qualified Data.Sized.Builtin as Sized

import Data.Singletons (Sing, SomeSing(SomeSing), toSing)

import GHC.TypeLits (Nat, type (+))

import Numeric.Natural (Natural)


data Hint a = Hint { hintRun :: Natural, hintValue :: a }
  deriving (Eq, Show)

$(makeFields ''Hint)


type Cell a = Maybe a

type List n a = Sized [] n a


newtype Grid (r :: Nat) (c :: Nat) (a :: *)
  = Grid { unGrid :: List r (List c a) }
  deriving (Eq, Show)


data Puzzle :: * -> *
  where Puzzle     :: { unPuzzle :: Grid (r :: Nat) (c :: Nat) (a :: *)
                      } -> Puzzle a

deriving instance Show a => Show (Puzzle a)


toRawLists :: Grid r c a -> [[a]]
toRawLists = Sized.toList . Sized.map Sized.toList . unGrid


constGrid :: Sing r -> Sing c -> a -> Grid r c a
constGrid r c = Grid . Sized.replicate r . Sized.replicate c


instance Eq a => Eq (Puzzle a)
  where Puzzle g == Puzzle h = toRawLists g == toRawLists h


instance Functor Puzzle
  where fmap f (Puzzle (Grid v)) = Puzzle . Grid . Sized.map (Sized.map f) $ v


initPuzzle :: a -> [Natural] -> [Natural] -> Puzzle a
initPuzzle cell rowHints colHints
  = let nRows = fromIntegral $ length rowHints
        nCols = fromIntegral $ length colHints
    in  case (toSing nRows, toSing nCols) of
          (SomeSing r, SomeSing c) -> Puzzle $ constGrid r c cell


inferLine :: Eq a => [Hint a] -> List n a -> [List n a]
inferLine [] = _


transform
 :: (List 0 a -> List 0 b)
 -> (forall n. (List n a -> List n b) -> (List (n + 1) a -> List (n + 1) b))
 -> (List m a -> List m b)
transform base _induct l@Sized.NilL = base l
transform base induct l@(_ :< _) = induct (transform base induct) l
