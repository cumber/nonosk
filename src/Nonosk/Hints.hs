{-# LANGUAGE DeriveFunctor
           , FlexibleInstances
           , GADTs
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeApplications
  #-}

module Nonosk.Hints
  ( Hint (..)
  , Hints, CappedHints
  , value
  , run
  , makeHint
  , someHint
  )
where

import Control.Lens ( Lens )

import Data.Constraint ( (:-) (Sub)
                       , Dict (Dict)
                       )

import Numeric.Natural ( Natural )


import Data.Indexed.Capped ( Capped )

import Data.Indexed.ForAnyKnownIndex ( ForAnyKnownIndex (instAnyKnownIndex)
                                     , ForAnyKnownIndexF (instAnyKnownIndexF)
                                     )

import Data.Indexed.Index ( Index (Index)
                          , index'
                          )

import Data.Indexed.Nat ( KnownNat )

import Data.Indexed.Some ( Some (Some)
                         , withSome
                         , someIndex
                         )

import Data.Indexed.SumList ( SumList )


-- | A Hint identifies a run of cells filled with a constant value.
data Hint n a
  where Hint :: KnownNat n => !a -> Hint n a

deriving instance Eq a => Eq (Hint n a)


deriving instance Functor (Hint n)

instance ForAnyKnownIndexF Functor Hint
  where instAnyKnownIndexF = Sub Dict


instance Show a => Show (Hint n a)
  where showsPrec p (Hint x)
          = showParen (p > appPrec)
              ( showString "Hint @ "
              . showsPrec (appPrec + 1) (index' @ n)
              . showString " "
              . showsPrec (appPrec + 1) x
              )
          where appPrec = 10

instance Show a => ForAnyKnownIndex Show Hint a
  where instAnyKnownIndex = Sub Dict


makeHint :: Index n () -> a -> Hint n a
makeHint Index = Hint

value :: Lens (Hint n a) (Hint n b) a b
value f (Hint x) = fmap Hint (f x)

run :: Lens (Hint n a) (Hint m a) (Index n ()) (Index m ())
run f (Hint x) = flip makeHint x <$> f Index

someHint :: Natural -> a -> Some Hint a
someHint n a = withSome (someIndex n) (Some . flip makeHint a)


type Hints sum a = SumList Hint sum a
type CappedHints length a = Capped length (SumList Hint) a
