{-# LANGUAGE ScopedTypeVariables #-}

module Test.Data.Indexed.Vector
  ( tests )
where

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
                         , withSome
                         )

import qualified Data.Indexed.Vector as Vector

import Scaffolding.SmallCheck ( vectorElements
                              )


tests :: TestTree
tests = testGroup "Vector" [ scProps
                           ]


scProps
  = testGroup "SmallCheck"
      [ appendList
      , replicateList
      ]


appendList
  = SC.testProperty "toList (Vector.append xs ys) == toList xs ++ toList ys" test
  where test :: Some Index () -> Some Index () -> Bool
        test x y
          = let xs = vectorElements x
                ys = vectorElements y
             in withSome xs (\xs' -> withSome ys (toList . Vector.append xs'))
                  == toList xs ++ toList ys

replicateList
  = SC.testProperty "toList . Vector.replicate == List.replicate" test
  where test :: Bool -> Some Index () -> Bool
        test x
          = forSome (\n -> toList (Vector.replicate n x)
                            == List.replicate (fromIntegral $ index n) x
                    )
