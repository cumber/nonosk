{-# LANGUAGE ConstraintKinds
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TypeInType
           , TypeOperators
           , UndecidableInstances
  #-}

module Scaffolding.SmallCheck
  ( tagged
  , taggedF
  )
where

import Test.SmallCheck.Series ( Serial (series)
                              , Series
                              , generate
                              )


import Scaffolding.Probe ( Probe (Probe, ProbeEnum)
                         , Tagged (Tagged)
                         )

import Scaffolding.TypeChoice ( TypeChoice
                              , Choosable
                              , choices
                              )

import Data.Indexed.Fin ( fromFin )

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


tagged :: Serial m (Probe cs) => String -> Series m (Tagged cs)
tagged tag = Tagged tag <$> series

taggedF :: (Serial m (f (Probe cs)), Functor f)
        => String -> Series m (f (Tagged cs))
taggedF tag = fmap (Tagged tag) <$> series


instance Monad m => Serial m (Some Index ())
  where series = generate $ \d -> if d < 0
                                    then  []
                                    else  someIndex <$> [0 .. fromIntegral d]


instance Monad m => Serial m (Some Vector (Probe '[]))
  where series = forSome (\i -> Some $ Vector.fromIndices i (Probe . fromFin))
                   <$> series

instance Monad m => Serial m (Some2 Vector2 (Probe '[]))
  where series
          = let makeRow len r = Vector.fromIndices len (Probe . pos len r)
                pos rowLenIndex rowNo c
                  = index rowLenIndex * (fromFin rowNo + fromFin c)
                makeVectors (Some rows, Some cols)
                  = Some2 . Vector2 $ Vector.fromIndices rows (makeRow cols)
             in makeVectors <$> series


instance Monad m => Serial m (Probe '[Enum])
  where series = ProbeEnum <$> series


instance (Monad m, Choosable c ts) => Serial m (TypeChoice c ts)
  where series = generate (`take` choices)
