{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver
                -fplugin GHC.TypeLits.Normalise
                -fplugin TypeNatSolver
  #-}
{-# LANGUAGE AllowAmbiguousTypes
           , DataKinds
           , DeriveFunctor
           , FlexibleContexts
           , GADTs
           , RankNTypes
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeApplications
           , TypeOperators
  #-}
module Data.Indexed.Trie
  ( PathTrie (..)
  , Link (..)
  , completePaths
  , incompletePaths
  , isEmpty
  , prune
  , checkPrefixesWithinChoiceDepth
  , removeDeadEndsWithinChoiceDepth
  )
where

import Control.Applicative ( Alternative ((<|>), empty ) )

import Data.Semigroup ( (<>) )

import Numeric.Natural ( Natural )


import Data.Indexed.Index ( Index (Index)
                          , switchZero'
                          )

import Data.Indexed.Nat ( KnownNat
                        , type (<)
                        , type (>=)
                        , type (+), type (-)
                        )

import Data.Indexed.Vector ( Vector ((:^), Nil) )


data Link n a = a :--> PathTrie (n - 1) a
  deriving (Show, Eq, Functor)


data PathTrie n a
  where Fork :: (n >= 1) => [Link n a] -> PathTrie n a
        Complete :: PathTrie 0 a

deriving instance Eq a => Eq (PathTrie n a)
deriving instance Show a => Show (PathTrie n a)
deriving instance Functor (PathTrie n)


instance KnownNat n => Applicative (PathTrie n)
  where pure x
          = switchZero' @ n
              Complete
              (Fork [x :--> (pure x)])

        (<*>) = switchZero' @ n
                  apComplete
                  apFork

apComplete :: PathTrie 0 (a -> b) -> PathTrie 0 a -> PathTrie 0 b
Complete `apComplete` Complete = Complete

apFork :: forall n a b
        . (KnownNat n, n >= 1)
       => PathTrie n (a -> b) -> PathTrie n a -> PathTrie n b
Fork fs `apFork` Fork xs
  = Fork $ do f :--> ft <- fs
              x :--> xt <- xs
              pure $ f x :--> switchZero' @ n (Fork []) (ft <*> xt)


instance KnownNat n => Alternative (PathTrie n)
  where empty
          = switchZero' @ n
              Complete
              (Fork [])

        Fork xs <|> Fork ys = Fork (xs <> ys)
        Complete <|> Complete = Complete


completePaths :: PathTrie n a -> [Vector n a]
completePaths Complete = [Nil]
completePaths (Fork ls) = linkPaths =<< ls
  where linkPaths (x :--> t) = (x :^) <$> completePaths t


incompletePaths :: PathTrie n a -> [[a]]
incompletePaths Complete = [[]]
incompletePaths (Fork []) = [[]]
incompletePaths (Fork ls@(_ : _)) = linkPaths =<< ls
  where linkPaths (x :--> t) = (x :) <$> incompletePaths t


prune :: forall i n a
       . (KnownNat n, i < n)
      => Index i () -> (a -> Bool) -> PathTrie n a -> PathTrie n a
prune Index = prune' @ i

prune' :: forall i n a
        . (KnownNat i, KnownNat n, i < n)
       => (a -> Bool) -> PathTrie n a -> PathTrie n a
prune' p (Fork ls)
  = Fork $ switchZero' @ i
             (filter (\(x :--> _) -> not $ p x) ls)
             (push (prune' @ (i - 1) @ (n - 1) p) <$> ls)


push :: (PathTrie n a -> PathTrie n a) -> (Link (n + 1) a -> Link (n + 1) a)
push f (x :--> t)
  = x :--> f t


isEmpty :: PathTrie n a -> Bool
isEmpty (Fork []) = True
isEmpty _ = False


checkPrefixesWithinChoiceDepth
  :: Natural -> PathTrie n a -> ([[a]], PathTrie n a)
checkPrefixesWithinChoiceDepth _ Complete = ([[]], Complete)
checkPrefixesWithinChoiceDepth n trie@(Fork choices)
  | [] <- choices
  = ([], Fork [])

  | [l] <- choices
  = case checkPrefixesWithinChoiceDepthLink n l
      of (prefixes, l') -> (prefixes, Fork [l'])

  | (_ : _ : _) <- choices
  = if n > 0
      then ( combineFollows
           . filter (not . null . fst)
           . map (checkPrefixesWithinChoiceDepthLink (n - 1))
           $ choices
           )
      else ([[]], trie)
  where combineFollows cs = (concatMap fst cs, Fork . map snd $ cs)


checkPrefixesWithinChoiceDepthLink
  :: Natural -> Link n a -> ([[a]], Link n a)
checkPrefixesWithinChoiceDepthLink n (x :--> t)
  = case checkPrefixesWithinChoiceDepth n t
      of (xss, t')  -> (fmap (x :) xss, x :--> t')


removeDeadEndsWithinChoiceDepth
  :: Natural -> PathTrie n a -> PathTrie n a
removeDeadEndsWithinChoiceDepth
  = (fmap . fmap) snd checkPrefixesWithinChoiceDepth
