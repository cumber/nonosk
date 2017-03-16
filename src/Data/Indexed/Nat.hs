{-# LANGUAGE ConstraintKinds
           , DataKinds
           , TypeFamilies
           , TypeOperators
  #-}

module Data.Indexed.Nat
  ( type (<=), type (>=)
  , type (<), type (>)
  )
where

import GHC.TypeLits ( type (<=?)
                    , type (<=)
                    )

type n > m = (n <=? m) ~ False
type n < m = m > n
type n >= m = m <= n
