{-# LANGUAGE ExplicitNamespaces
           , GADTs
           , PatternSynonyms
           , ScopedTypeVariables
  #-}

module Data.Indexed.Index
  ( Index (..)
  , type I
  , pattern I
  )
where

import Data.Void ( Void )

import GHC.TypeLits ( KnownNat
                    , natVal
                    )

data Index n a
  where Index :: KnownNat n => Index n a

type I n = Index n Void

pattern I :: () => KnownNat n => I n
pattern I = Index


instance Show (Index n a)
  where showsPrec p Index
          = showParen (p > appPrec)
              ( showString "Index @ "
              . showsPrec (appPrec + 1) (natVal (undefined :: p n))
              )
          where appPrec = 10
