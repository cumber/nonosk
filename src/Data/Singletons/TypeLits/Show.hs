{-# LANGUAGE FlexibleInstances
           , StandaloneDeriving
           , TypeInType
  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Singletons.TypeLits.Show
  ()
where

import Data.Singletons (SomeSing (SomeSing))
import Data.Singletons.TypeLits
  ( Sing (SNat, SSym)
  , SNat
  , Nat
  , natVal
  , SSymbol
  , Symbol
  , symbolVal
  )

instance Show (SNat n)
  where showsPrec p n@SNat
          = showParen (p > atPrec)
              ( showString "SNat @ "
              . showsPrec (atPrec + 1) (natVal n)
              )
          where atPrec = 10


instance Show (SSymbol s)
  where showsPrec p s@SSym
          = showParen (p > atPrec)
              ( showString "SSym @ "
              . showsPrec (atPrec + 1) (symbolVal s)
              )
          where atPrec = 10


deriving instance Show (SomeSing Nat)
deriving instance Show (SomeSing Symbol)
