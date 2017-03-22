{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
  #-}

module Scaffolding.SmallCheck
  ( Element
  , vector
  , list
  )
where

import Data.List ( inits )

import GHC.Natural ( Natural )

import Test.SmallCheck.Series ( Serial (series)
                              , Series
                              , generate
                              )


import Data.Indexed.Index ( Index
                          , someIndex
                          )

import Data.Indexed.Some ( Some (Some)
                         , forSome
                         )

import Data.Indexed.Vector ( Vector
                           , fromIndices
                           )


instance Monad m => Serial m (Some Index ())
  where series = generate $ \d -> someIndex <$> [0 .. fromIntegral d]


-- | Helper type for writing tests for polymorphic functions; the only exported
--   functionality is equality testing.
data Element = Element String Natural
  deriving (Eq)

instance Show Element
  where show (Element tag index) = tag ++ show index


vector :: Monad m => String -> Series m (Some Vector Element)
vector tag = forSome (\i -> Some $ fromIndices i (Element tag)) <$> series

list :: Monad m => String -> Series m [Element]
list tag = generate $ \d -> inits $ Element tag <$> [0 .. fromIntegral d]
