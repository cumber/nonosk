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
                         )

import qualified Data.Indexed.Vector as Vector

import Scaffolding.SmallCheck ()


tests :: TestTree
tests = testGroup "Vector" [ scProps
                           ]


scProps
  = testGroup "SmallCheck"
      [ replicateList
      ]


replicateList
  = SC.testProperty "toList . Vector.replicate == List.replicate" test
  where test :: Bool -> Some Index () -> Bool
        test x
          = forSome (\n -> toList (Vector.replicate n x)
                            == List.replicate (fromIntegral $ index n) x
                    )
