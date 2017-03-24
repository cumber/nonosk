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

import Data.Indexed.Vector ( Vector )
import qualified Data.Indexed.Vector as Vector

import Data.Indexed.Vector2 ( Vector2 (Vector2) )

import Scaffolding.SmallCheck ( taggedF )
import Scaffolding.Probe ( Probe
                         , Tagged
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

      , indexLengthFoldable

      , transposeList

      , fromListToList
      , fromListIndexedToList
      , enumerateList
      , replicateList
      ]


testAgainstList
  :: Eq r
  =>    String
     -> (forall n. KnownNat n => Vector n (Probe '[]) -> r)
     -> ([Probe '[]] -> r)
     -> TestTree
testAgainstList label vectorOp listOp
  = SC.testProperty label test
  where test (Some xs) = vectorOp xs == (listOp . toList) xs


testAgainstNonEmptyList
  :: Eq r
  =>    String
     -> (forall n. (KnownNat n, n >= 1) => Vector n (Probe '[]) -> r)
     -> ([Probe '[]] -> r)
     -> TestTree
testAgainstNonEmptyList label vectorOp listOp
  = SC.testProperty label test
  where test :: Some Vector (Probe '[]) -> Bool
        test (Some (xs :: Vector n (Probe '[])))
          = switchZero' @ n
              (null . toList $ xs)
              (vectorOp xs == (listOp . toList) xs)


over2 s1 s2 q = SC.over s1 (\x -> SC.over s2 (\y -> q x y))


appendList
  = SC.testProperty "Vector.append xs ys  vs  toList xs List.++ toList ys"
      $ over2 (taggedF "X") (taggedF "Y") test
  where test :: Some Vector (Tagged '[]) -> Some Vector (Tagged '[]) -> Bool
        test (Some xs) (Some ys)
          = toList (Vector.append xs ys) == toList xs List.++ toList ys

appendOperatorList
  = SC.testProperty "xs Vector.++ ys  vs  toList xs List.++ toList ys"
      $ over2 (taggedF"X") (taggedF"Y") test
  where test :: Some Vector (Tagged '[]) -> Some Vector (Tagged '[]) -> Bool
        test (Some xs) (Some ys)
          = toList (xs Vector.++ ys) == toList xs List.++ toList ys


indexLengthFoldable
  = SC.testProperty "Vector.indexLength  vs  Foldable.length" test
  where test :: Some Vector (Probe '[]) -> Bool
        test (Some xs)
          = (fromIntegral . index . Vector.indexLength) xs == length xs

transposeList
  = SC.testProperty "Vector.transpose  vs  List.transpose" test
  where test :: Some2 Vector2 (Probe '[]) -> Bool
        test (Some2 (Vector2 xss :: Vector2 r c (Probe '[])))
          = let v = (fmap toList . toList . Vector.transpose) xss
                l = (List.transpose . fmap toList . toList) xss
                -- Vector.transpose faithfully transforms 0 rows of n columns
                -- into n empty rows, whereas List.transpose has no way to
                -- tell any of the zero row values of type [[a]] apart, so
                -- always produces []
                emptyRows= List.genericReplicate (index' @ c) []
             in v == l
                 || (  index' @ r == 0
                    && index' @ c >= 1
                    && v == emptyRows
                    )


fromListToList
  = SC.testProperty "Vector.fromList . toList" test
  where test :: Some Vector (Probe '[]) -> Bool
        test (Some xs) = (Vector.fromList . toList) xs == Some xs

fromListIndexedToList
  = SC.testProperty "Vector.fromListIndexed . toList" test
  where test :: Some Vector (Probe '[]) -> Bool
        test (Some xs)
          = (Vector.fromListIndexed (Vector.indexLength xs) . toList) xs
             == Just xs

replicateList
  = SC.testProperty "Vector.replicate  vs  List.replicate" test
  where test :: Bool -> Some Index () -> Bool
        test x (Some n)
          = toList (Vector.replicate n x)
             == List.replicate (fromIntegral $ index n) x

enumerateList
  = SC.testProperty "Vector.enumerate  vs  enumFrom" test
  where test :: Some Index () -> Probe '[Enum] -> Bool
        test (Some n) start
          = toList (Vector.enumerate n start)
             == List.genericTake (index n) (enumFrom start)
