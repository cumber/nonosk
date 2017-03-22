{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
  #-}

module Scaffolding.SmallCheck
  ( Element
  , Element2
  , vector
  , vector2
  )
where

import GHC.Natural ( Natural )

import Test.SmallCheck.Series ( Serial (series)
                              , Series
                              , generate
                              )


import Data.Indexed.Index ( Index
                          , someIndex
                          )

import Data.Indexed.Some ( Some (Some)
                         , Some2 (Some2)
                         , forSome
                         )

import Data.Indexed.Vector ( Vector
                           , fromIndices
                           )
import Data.Indexed.Vector2 ( Vector2 (Vector2) )


instance Monad m => Serial m (Some Index ())
  where series = generate $ \d -> someIndex <$> [0 .. fromIntegral d]


-- | Helper type for writing tests for polymorphic functions; the only exported
--   functionality is equality testing.
data Element = Element String Natural
  deriving (Eq)

instance Show Element
  where show (Element tag index) = tag ++ show index


data Element2 = Element2 String Natural Natural
  deriving (Eq)

instance Show Element2
  where show (Element2 tag x y) = tag ++ "_" ++ show x ++ "_" ++ show y


vector :: Monad m => String -> Series m (Some Vector Element)
vector tag = forSome (\i -> Some $ fromIndices i (Element tag)) <$> series

vector2 :: Monad m => String -> Series m (Some2 Vector2 Element2)
vector2 tag
  = let makeRow len r = fromIndices len (Element2 tag r)
        makeVectors (Some rows, Some cols)
          = Some2 . Vector2 $ fromIndices rows (makeRow cols)
     in makeVectors <$> series
