{-# OPTIONS_GHC -fplugin TypeNatSolver #-}
{-# LANGUAGE AllowAmbiguousTypes
           , ConstraintKinds
           , DataKinds
           , ExplicitForAll
           , TypeFamilies
           , TypeOperators
  #-}

module Data.Indexed.Util.NatProofs
  ( ltSuccLe
  , gtGeSucc
  , minusMonotone1
  )
where

import Data.Constraint ( (:-) (Sub)
                       , Dict (Dict)
                       )


import Data.Indexed.Nat ( type (+), type (-)
                        , type (<=), type (>=)
                        , type (<), type (>)
                        )


ltSuccLe :: forall a b. (a < b) :- ((a + 1) <= b)
ltSuccLe = Sub Dict

gtGeSucc :: forall a b. (a > b) :- (a >= (b + 1))
gtGeSucc = Sub Dict


minusMonotone1 :: forall a b c. (c <= a, c <= b, a <= b) :- ((a - c) <= (b - c))
minusMonotone1 = Sub Dict
