{-# LANGUAGE ConstraintKinds
           , DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , StandaloneDeriving
           , TypeFamilies
           , UndecidableInstances
  #-}

module Scaffolding.Probe
  ( Tagged (..)
  , tagged
  , taggedF
  , Probe ()
  )
where

import Data.Constraint ( Constraint )

import Data.Kind ( Type )

import GHC.Natural ( Natural )

import Test.SmallCheck.Series ( Serial (series)
                              , Series
                              , generate
                              )


import Data.Indexed.Index ( Index 
                          , index
                          )

import Data.Indexed.Some ( Some (Some)
                         , forSome
                         , Some2 (Some2)
                         , someIndex
                         )

import Data.Indexed.Vector ( Vector )
import qualified Data.Indexed.Vector as Vector

import Data.Indexed.Vector2 ( Vector2 (Vector2) )
import qualified Data.Indexed.Vector as Vector2


data Tagged cs = Tagged String (Probe cs)

deriving instance Eq (Probe cs) => Eq (Tagged cs)

instance Show (Probe cs) => Show (Tagged cs)
  where show (Tagged tag p) = tag ++ show p


tagged :: Serial m (Probe cs) => String -> Series m (Tagged cs)
tagged tag = Tagged tag <$> series

taggedF :: (Serial m (f (Probe cs)), Functor f)
        => String -> Series m (f (Tagged cs))
taggedF tag = fmap (Tagged tag) <$> series


instance Monad m => Serial m (Some Index ())
  where series = generate $ \d -> if d < 0
                                    then  []
                                    else  someIndex <$> [0 .. fromIntegral d]

data family Probe (cs :: [Type -> Constraint])


-- Probe supporting no operations; only identity testing
newtype instance Probe '[] = Probe Natural
  deriving (Eq)

instance Show (Probe '[])
  where show (Probe x) = "#" ++ show x

instance Monad m => Serial m (Some Vector (Probe '[]))
  where series = forSome (\i -> Some $ Vector.fromIndices i Probe) <$> series

instance Monad m => Serial m (Some2 Vector2 (Probe '[]))
  where series
          = let makeRow len r = Vector.fromIndices len (Probe . pos len r)
                pos rowLenIndex rowNo c = index rowLenIndex * rowNo + c
                makeVectors (Some rows, Some cols)
                  = Some2 . Vector2 $ Vector.fromIndices rows (makeRow cols)
             in makeVectors <$> series


-- Probe supporting enum, where (succ . pred) == (pred . succ) == id
newtype instance Probe '[Enum] = ProbeEnum Integer
  deriving (Eq, Enum)

instance Show (Probe '[Enum])
  where show (ProbeEnum x) = "#" ++ show x

instance Monad m => Serial m (Probe '[Enum])
  where series = ProbeEnum <$> series
