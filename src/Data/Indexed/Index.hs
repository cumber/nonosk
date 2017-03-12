{-# LANGUAGE AllowAmbiguousTypes
           , DataKinds
           , GADTs
           , MagicHash
           , RankNTypes
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeApplications
           , TypeOperators
  #-}

module Data.Indexed.Index
  ( Index' (..)
  , Index
  , IsZero (..)
  , indexVal
  , isZero
  , switchZero
  )
where

import Data.Constraint ( Dict (Dict) )

import Data.Void ( Void )

import GHC.Prim ( proxy# )

import GHC.Natural ( Natural )

import GHC.TypeLits ( type (<=)
                    , KnownNat
                    , Nat
                    , natVal'
                    )

import Unsafe.Coerce ( unsafeCoerce )

data Index' n a
  where Index :: KnownNat n => Index' n a

type Index n = Index' n Void


indexVal :: forall n a. Index' n a -> Natural
indexVal Index = indexVal' @ n


indexVal' :: forall n. KnownNat n => Natural
indexVal' = fromIntegral $ natVal' (proxy# @ Nat @ n)


instance Show (Index' n a)
  where showsPrec p i
          = showParen (p > appPrec)
              ( showString "Index @ "
              . showsPrec (appPrec + 1) (indexVal i)
              )
          where appPrec = 10


data IsZero n
  where Zero :: (0 ~ n) => IsZero n
        NonZero :: (1 <= n) => IsZero n

deriving instance Show (IsZero n)


isZero :: Index n -> IsZero n
isZero n = switchZero n Zero NonZero


switchZero :: forall n r. Index n -> ((0 ~ n) => r) -> ((1 <= n) => r) -> r
switchZero n zero nonzero
  = if indexVal n == 0
      then case unsafeCoerce (Dict :: Dict (n ~ n))
             of (Dict :: Dict (0 ~ n)) -> zero
      else case unsafeCoerce (Dict :: Dict (0 <= n))
             of (Dict :: Dict (1 <= n)) -> nonzero
