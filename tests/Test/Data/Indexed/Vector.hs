{-# LANGUAGE DataKinds
           , RankNTypes
           , ScopedTypeVariables
           , TypeApplications
           , TypeFamilies
           , TypeOperators
  #-}

module Test.Data.Indexed.Vector
  ( tests )
where

import Control.Arrow ( second )

import Data.Foldable ( toList )

import qualified Data.List as List

import Test.Tasty ( TestTree
                  , testGroup
                  )

import qualified Test.Tasty.SmallCheck as SC


import Data.Indexed.Index ( Index
                          , index
                          , index'
                          , switchZero'
                          )

import Data.Indexed.Nat ( KnownNat
                        , type (>=)
                        )

import Data.Indexed.Some ( Some (Some)
                         , Some2 (Some2)
                         )

import Data.Indexed.Vector ( Vector (Nil) )
import qualified Data.Indexed.Vector as Vector

import Data.Indexed.Vector2 ( Vector2 (Vector2) )

import Scaffolding.SmallCheck ( Element
                              , Element2
                              , vector
                              , vector2
                              )


tests :: TestTree
tests = testGroup "Vector" [ scProps
                           ]


scProps
  = testGroup "SmallCheck"
      [ appendList
      , appendOperatorList

      , testAgainstNonEmptyList "Vector.head  vs  List.head"
          (Vector.head) (List.head)
      , testAgainstNonEmptyList "Vector.last  vs  List.last"
          (Vector.last) (List.last)
      , testAgainstNonEmptyList "Vector.tail  vs  List.tail"
          (toList . Vector.tail) (List.tail)
      , testAgainstNonEmptyList "Vector.init  vs  List.init"
          (toList . Vector.init) (List.init)
      , testAgainstNonEmptyList "Vector.init vs List.init"
          (Just . second toList . Vector.uncons) (List.uncons)

      , transposeList

      , replicateList
      ]


testAgainstList
  :: Eq r
  =>    String
     -> (forall n. KnownNat n => Vector n Element -> r)
     -> ([Element] -> r)
     -> TestTree
testAgainstList label vectorOp listOp
  = SC.testProperty label $ SC.over (vector "X") test
  where test (Some xs) = vectorOp xs == (listOp . toList) xs


testAgainstNonEmptyList
  :: Eq r
  =>    String
     -> (forall n. (KnownNat n, n >= 1) => Vector n Element -> r)
     -> ([Element] -> r)
     -> TestTree
testAgainstNonEmptyList label vectorOp listOp
  = SC.testProperty label $ SC.over (vector "X") test
  where test :: Some Vector Element -> Bool
        test (Some (xs :: Vector n Element))
          = switchZero' @ n
              (null . toList $ xs)
              (vectorOp xs == (listOp . toList) xs)


over2 s1 s2 q = SC.over s1 (\x -> SC.over s2 (\y -> q x y))


appendList
  = SC.testProperty "Vector.append xs ys  vs  toList xs ++ toList ys"
      $ over2 (vector "X") (vector "Y") test
  where test :: Some Vector Element -> Some Vector Element -> Bool
        test (Some xs) (Some ys)
          = toList (Vector.append xs ys) == toList xs ++ toList ys

appendOperatorList
  = SC.testProperty "xs Vector.++ ys  vs  toList xs ++ toList ys"
      $ over2 (vector "X") (vector "Y") test
  where test :: Some Vector Element -> Some Vector Element -> Bool
        test (Some xs) (Some ys)
          = toList (xs Vector.++ ys) == toList xs ++ toList ys


transposeList
  = SC.testProperty "Vector.transpose  vs  List.transpose"
      $ SC.over (vector2 "X") test
  where test :: Some2 Vector2 Element2 -> Bool
        test (Some2 (Vector2 xss :: Vector2 r c Element2))
          = let v = (fmap toList . toList . Vector.transpose) xss
                l = (List.transpose . fmap toList . toList) xss
                -- Vector.transpose faithfully transforms 0 rows of n columns
                -- into n empty rows, whereas List.transpose has no way to
                -- tell any of the zero row values of type [[a]] apart, so
                -- always produces []
                emptyRows= List.genericReplicate (index' @ c) []
             in v == l || (index' @ r == 0 && index' @ c >= 1 && v == emptyRows)


replicateList
  = SC.testProperty "Vector.replicate  vs  List.replicate" test
  where test :: Bool -> Some Index () -> Bool
        test x (Some n)
          = toList (Vector.replicate n x)
              == List.replicate (fromIntegral $ index n) x
