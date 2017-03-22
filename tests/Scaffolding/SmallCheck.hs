{-# LANGUAGE FlexibleInstances
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
  #-}

module Scaffolding.SmallCheck
  ( identifiedElements
  , vectorElements
  )
where

import qualified Data.List as List

import GHC.Natural (Natural)

import Test.SmallCheck.Series ( Serial (series)
                              , generate
                              )


import Data.Indexed.Index ( Index
                          , index
                          , someIndex
                          )

import Data.Indexed.Some ( Some (Some) )

import Data.Indexed.Vector ( Vector
                           , fromIndices
                           )


instance Monad m => Serial m (Some Index ())
  where series = generate $ \d -> someIndex <$> [0 .. fromIntegral d]


newtype Element = Element Natural
  deriving (Eq)

elements :: [Element]
elements = Element <$> [0..]

vectorElements :: Some Index () -> Some Vector Element
vectorElements (Some i) = Some $ fromIndices i Element

identifiedElements :: Some Index () -> (Some Vector Element, [Element])
identifiedElements (Some i)
  = (vectorElements (Some i), List.genericTake (index i) elements)
