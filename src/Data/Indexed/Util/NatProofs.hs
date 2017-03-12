{-# OPTIONS_GHC -fplugin TypeNatSolver #-}
{-# LANGUAGE AllowAmbiguousTypes
           , ExplicitForAll
           , TypeOperators
  #-}

module Data.Indexed.Util.NatProofs
where

import Data.Constraint ( (:-) (Sub)
                       , Dict (Dict)
                       )

import GHC.TypeLits ( type (-)
                    , type (<=)
                    )

minusMonotone1 :: forall a b c. (c <= a, c <= b, a <= b) :- ((a - c) <= (b - c))
minusMonotone1 = Sub Dict
