{-# LANGUAGE RankNTypes
           , ScopedTypeVariables
           , TypeApplications
           , UndecidableInstances
  #-}

module Scaffolding.Poly
  ( Poly (Poly)
  , Val
  , fillPoly
  , fillPolyTagged
  , tagValues
  )
where

import Data.Coerce ( coerce )

import Numeric.Natural ( Natural )


-- Probe type with unexported constructors; only supports distinguishing
-- original elements, and show for convenience in showing counterexamples
data Val = Val { _tag :: String
               , _index :: Natural
               }
  deriving (Eq)

instance Show Val
  where show (Val tag x) = tag ++ "#" ++ show x


-- A Poly f is intended as a way of testing functions on f a that are
-- polymorphic in a. The constructor Poly is exported, but not the constructor
-- of Val; since no way to construct Vals (other than fillPoly) is exported,
-- when intending to write tests that only manipulate Vals with polymorphic
-- functions, it's difficult to accidentally use Val-specific functionality.
newtype Poly f = Poly (f Val)

instance Show (f Val) => Show (Poly f)
  where show (Poly f) = show f


-- A function that could take elements from an infinite list of unique values
-- and build an f a, can be used to build a Poly f. Since Poly's constructor is
-- unexported, this is the only way to get one.
fillPoly :: (forall a. [a] -> f a) -> Poly f
fillPoly = fillPolyTagged ""

fillPolyTagged :: String -> (forall a. [a] -> f a) -> Poly f
fillPolyTagged tag fill
  = Poly . fill $ Val tag <$> [0 ..]


tagValues :: forall f. Functor f => String -> Poly f -> Poly f
tagValues tag = coerce (fmap @f (\v -> v { _tag = tag }))
