{-# LANGUAGE ConstraintKinds
           , DataKinds
           , TypeFamilies
           , TypeOperators
  #-}

module Data.Indexed.Nat
  ( Nat, KnownNat
  , type (+), type (-), type (*), type (×)
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

-- The multiplication type operator confuses a lot of tooling (haskell-src-exts,
-- hlint, etc) due to special parse rules needed for the kind * (Type).
-- Export a unicode equivalnet for convenience.
type a × b = a GHC.TypeLits.* b
