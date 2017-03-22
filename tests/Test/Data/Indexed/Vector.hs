{-# LANGUAGE ScopedTypeVariables #-}

module Test.Data.Indexed.Vector
  ( tests )
where

import Control.Monad ( (<=<) )
import Data.Foldable ( toList )

import qualified Data.List as List

import Test.Tasty ( TestTree
                  , testGroup
                  )

import qualified Test.Tasty.SmallCheck as SC


import Data.Indexed.Index ( Index
                          , index
                          )

import Data.Indexed.Some ( Some
                         , forSome
                         , liftPlus
                         )

import Data.Indexed.Vector ( Vector )
import qualified Data.Indexed.Vector as Vector

import Scaffolding.SmallCheck ( Element
                              , vector
                              )


tests :: TestTree
tests = testGroup "Vector" [ scProps
                           ]


scProps
  = testGroup "SmallCheck"
      [ appendList
      , replicateList
      ]


over2 s1 s2 q = SC.over s1 (\x -> SC.over s2 (\y -> q x y))


appendList
  = SC.testProperty "toList (Vector.append xs ys) == toList xs ++ toList ys"
      $ over2 (vector "X") (vector "Y") test
  where test :: Some Vector Element -> Some Vector Element -> Bool
        test xs ys
          = toList (liftPlus Vector.append xs ys) == toList xs ++ toList ys ++ toList ys

replicateList
  = SC.testProperty "toList . Vector.replicate == List.replicate" test
  where test :: Bool -> Some Index () -> Bool
        test x
          = forSome (\n -> toList (Vector.replicate n x)
                            == List.replicate (fromIntegral $ index n) x
                    )
