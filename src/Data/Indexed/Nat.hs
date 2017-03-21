{-# LANGUAGE ConstraintKinds
           , DataKinds
           , TypeFamilies
           , TypeOperators
  #-}

module Data.Indexed.Nat
  ( Nat, KnownNat
  , type (+), type (-), type (*)
  , type (<=), type (>=)
  , type (<), type (>)
  )
where

import GHC.TypeLits ( Nat, KnownNat
                    , type (+), type (-), type (*)
                    , type (<=?), type (<=)
                    )

type n > m = (n <=? m) ~ False
type n < m = m > n
type n >= m = m <= n
