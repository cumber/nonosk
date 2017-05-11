{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , UndecidableInstances
  #-}

module Scaffolding.SmallCheck
where

import Control.Applicative ( Alternative (empty) )

import qualified Data.IntSet as IntSet

import qualified Data.List as List

import Data.List.HT ( sliceVertical )

import Data.Maybe ( fromMaybe )

import Math.NumberTheory.ArithmeticFunctions ( divisorsSmall )
import Math.NumberTheory.Powers.Squares ( integerSquareRoot )

import Numeric.Natural ( Natural )

import Test.SmallCheck.Series ( Serial (series)
                              , Series
                              , generate
                              , localDepth
                              )


import Scaffolding.Poly ( Poly
                        , fillPoly
                        , tagValues
                        )

import Scaffolding.TypeChoice ( TypeChoice
                              , Choosable
                              , choices
                              )

import Data.Indexed.Index ( Index )

import Data.Indexed.Some ( Some
                         , Some2
                         , someIndex
                         )

import Data.Indexed.Vector ( Vector )
import qualified Data.Indexed.Vector as Vector

import Data.Indexed.Vector2 ( Vector2 )
import qualified Data.Indexed.Vector2 as Vector2


instance Monad m => Serial m (Some Index ())
  where series = someIndex <$> naturals


polyVector :: Natural -> Poly (Some Vector)
polyVector n = fillPoly (Vector.fromList . List.genericTake n)

polyVector2 :: Natural -> Natural -> Poly (Some2 Vector2)
polyVector2 r c = fillPoly (unjust . Vector2.fromLists . unflatten)
  where unflatten = List.genericTake r . sliceVertical (fromIntegral c)
        unjust = fromMaybe (error "fillPoly should have given an infinite list")


naturals :: Series m Natural
naturals = generate $ \d -> if d < 0
                              then  []
                              else  [0 .. fromIntegral d]


instance (Monad m, Serial m a) => Serial m (Some Vector a)
  where series = Vector.fromList <$> series


instance Monad m => Serial m (Poly (Some Vector))
  where series = polyVector <$> naturals


instance (Monad m, Serial m a) => Serial m (Some2 Vector2 a)
  where series
          = do  elems <- sqrtDepth series
                let ds = divisorsSmall (length elems)
                rowLen <- generate (\d -> take d . IntSet.toList $ ds)
                pure . unjust . Vector2.fromLists $ sliceVertical rowLen elems
          where unjust = fromMaybe (error "rows should be even by construction")
                sqrtDepth = localDepth integerSquareRoot

instance Monad m => Serial m (Poly (Some2 Vector2))
  where series = polyVector2 <$> naturals <*> naturals


maybeAlternative :: Alternative f => Maybe a -> f a
maybeAlternative = maybe empty pure


tagged :: (Functor f, Serial m (Poly f)) => String -> Series m (Poly f)
tagged tag = tagValues tag <$> series


instance (Monad m, Choosable c ts) => Serial m (TypeChoice c ts)
  where series = generate (`take` choices)
