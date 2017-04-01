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
  , leLtSucc
  , geSuccGt
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

leLtSucc :: forall a b. (a <= b) :- (a < (b + 1))
leLtSucc = Sub Dict

geSuccGt :: forall a b. (a >= b) :- ((a + 1) > b)
geSuccGt = Sub Dict


minusMonotone1 :: forall a b c. (c <= a, c <= b, a <= b) :- ((a - c) <= (b - c))
minusMonotone1 = Sub Dict
