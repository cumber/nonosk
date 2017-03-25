{-# LANGUAGE ConstraintKinds
           , DataKinds
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , StandaloneDeriving
           , TypeFamilies
           , UndecidableInstances
  #-}

module Scaffolding.Probe
  ( Tagged (..)
  , Probe (..)
  )
where

import Data.Constraint ( Constraint )

import Data.Kind ( Type )

import Numeric.Natural ( Natural )


data family Probe (cs :: [Type -> Constraint])


data Tagged cs = Tagged String (Probe cs)

deriving instance Eq (Probe cs) => Eq (Tagged cs)

instance Show (Probe cs) => Show (Tagged cs)
  where show (Tagged tag p) = tag ++ show p




-- Probe supporting no operations; only identity testing
newtype instance Probe '[] = Probe Natural
  deriving (Eq)

instance Show (Probe '[])
  where show (Probe x) = "#" ++ show x


-- Probe supporting enum, where (succ . pred) == (pred . succ) == id
newtype instance Probe '[Enum] = ProbeEnum Integer
  deriving (Eq, Enum)

instance Show (Probe '[Enum])
  where show (ProbeEnum x) = "#" ++ show x
