{-# LANGUAGE GADTs
           , RankNTypes
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeApplications
           , TypeOperators
           , TypeInType
  #-}

module Data.Indexed.Index
  ( Index' (..)
  , Index
  , IsZero (..)
  --, isZero
  --, switchZero
  )
where

import Data.Void ( Void )

import GHC.TypeLits ( type (<=)
                    , type (-)
                    , KnownNat
                    , natVal
                    )

data Index' n a
  where Index :: KnownNat n => Index' n a

type Index n = Index' n Void


instance Show (Index' n a)
  where showsPrec p Index
          = showParen (p > appPrec)
              ( showString "Index @ "
              . showsPrec (appPrec + 1) (natVal (undefined :: p n))
              )
          where appPrec = 10


data IsZero n
  where Zero :: (0 ~ n) => IsZero n
        NonZero :: (1 <= n) => IsZero n

deriving instance Show (IsZero n)


{-
switchZero :: forall n r. Index n -> ((0 ~ n) => r) -> ((1 <= n) => r) -> r
switchZero = undefined -- n zero nonzero
  = case n %~ Index @ 0
      of Proved Refl -> zero
         Disproved _ -> nonzero \\ plusMonotone1 @ 0 @ (n - 1) @ 1


isZero :: Index n -> IsZero n
isZero n = switchZero n Zero NonZero
-}
