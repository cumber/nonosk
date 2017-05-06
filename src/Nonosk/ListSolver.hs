{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver
                -fplugin TypeNatSolver
  #-}
{-# LANGUAGE DataKinds
           , DeriveFunctor
           , FlexibleContexts
           , FlexibleInstances
           , GADTs
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeApplications
           , TypeFamilies
           , TypeOperators
  #-}

module Nonosk.ListSolver
  ( Solver
  , initSolver
  , makeSolver
  , solve
  , iterateSolver
  , inferGrid
  , inferLine
  , possibleLines
  )
where

import Control.Applicative ( Alternative ((<|>), empty) )

import Control.Lens ( (^.) )

import Control.Monad ( (<=<) )

import Control.Parallel.Strategies ( parTraversable
                                   , rseq
                                   , withStrategy
                                   )

import Data.Constraint ( (:-) (Sub)
                       , Dict (Dict)
                       , (\\)
                       )

import Data.List ( unfoldr )


import Data.Indexed.Capped ( tryCap
                           , forCapped
                           )

import Data.Indexed.ForAnyKnownIndex ( ForAnyKnownIndex2 (instAnyKnownIndex2) )

import Data.Indexed.Index ( switchZero' )

import Data.Indexed.Nat ( Nat, KnownNat
                        , type (+), type (-)
                        , type (<=)
                        )

import Data.Indexed.Some ( Some (Some)
                         , Some2 (Some2)
                         , guessIndex2'
                         )

import Data.Indexed.SumList ( SumList ((:+), EmptySum) )
import qualified Data.Indexed.SumList as SumList

import Data.Indexed.Vector ( Vector (Nil) )
import qualified Data.Indexed.Vector as Vector

import Data.Indexed.Vector2 ( Vector2 (Vector2) )
import qualified Data.Indexed.Vector2 as Vector2


import Nonosk.Hints
import Nonosk.Grid


data Solver :: Nat -> Nat -> * -> *
  where Solver :: { rowHints :: Vector r (CappedHints c (Cell a))
                  , colHints :: Vector c (CappedHints r (Cell a))
                  , solverGrid :: GridKnowledge r c a
                  } -> Solver r c a

deriving instance Functor (Solver r c)
deriving instance (KnownNat r, KnownNat c, Show a) => Show (Solver r c a)

instance Show a => ForAnyKnownIndex2 Show Solver a
  where instAnyKnownIndex2 = Sub Dict


initSolver :: Eq a => [[Some Hint a]] -> [[Some Hint a]] -> Maybe (Some2 Solver a)
initSolver extRowHints extColHints
  = case ( (vecSumFromLists $ fmap addSpacers extRowHints)
         , (vecSumFromLists $ fmap addSpacers extColHints)
         )
      of (Some rows, Some cols)
           -> let rows' = sequenceA $ fmap tryCap rows
                  cols' = sequenceA $ fmap tryCap cols
              in  Some2 <$> (Solver <$> rows'
                                    <*> cols'
                                    <*> pure (Vector2.replicate' Unknown)
                            )


makeSolver :: Eq a
           =>     [[Some Hint a]]
               -> [[Some Hint a]]
               -> Some2 Vector2 (Knowledge (Cell a))
               -> Maybe (Some2 Solver a)
makeSolver extRowHints extColHints extGrid
  = case ( (vecSumFromLists $ fmap addSpacers extRowHints)
         , (vecSumFromLists $ fmap addSpacers extColHints)
         )
      of (Some rows, Some cols)
           -> let rows' = sequenceA $ fmap tryCap rows
                  cols' = sequenceA $ fmap tryCap cols
              in  Some2 <$> (Solver <$> rows'
                                    <*> cols'
                                    <*> guessIndex2' extGrid
                            )


addSpacers :: Eq a => [Some Hint a] -> [Some Hint (Cell a)]
addSpacers [] = []
addSpacers (Some h : hs) = Some (fmap Filled h) : go (h ^. value) hs
  where go _ [] = []
        go prev (Some x : xs)
          | xv == prev  = Some (Hint @ 1 Empty) : rest
          | otherwise   = rest
          where xv = x ^. value
                rest = Some (fmap Filled x) : go xv xs


vecSumFromLists :: [[Some f a]] -> Some Vector (Some (SumList f) a)
vecSumFromLists = Vector.fromList . fmap SumList.fromSomeList


solve :: (KnownNat r, KnownNat c, Eq a)
      => Solver r c a -> Maybe (Grid r c a)
solve solver
  = let finalGrid = safeLast . inferGrid (rowHints solver) (colHints solver)
                             $ (solverGrid solver)
        checkSolved = sequenceA . fmap (withKnowledge Nothing Just)
     in checkSolved =<< finalGrid


safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x : xs) = Just $ go x xs
  where go y [] = y
        go _ (y : ys) = go y ys


iterateSolver :: (KnownNat r, KnownNat c, Eq a)
                 => Solver r c a -> [GridKnowledge r c a]
iterateSolver solver
  = inferGrid (rowHints solver) (colHints solver) (solverGrid solver)


inferGrid :: (KnownNat r, KnownNat c, Eq a)
            =>    Vector r (CappedHints c (Cell a))
               -> Vector c (CappedHints r (Cell a))
               -> GridKnowledge r c a -> [GridKnowledge r c a]
inferGrid rHints cHints
  = takeUntilDup . iterateMaybe doStep
  where doStep = inferRows rHints <=< inferColumns cHints


takeUntilDup :: Eq a => [a] -> [a]
takeUntilDup [] = []
takeUntilDup (x : xs) = x : go x xs
  where go _ [] = []
        go prev (y : ys)
          | prev == y  = []
          | otherwise  = y : go y ys


iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = unfoldr (fmap dup . f)
  where dup x = (x, x)


inferRows :: (KnownNat c, Eq a)
          =>    Vector r (CappedHints c (Cell a))
             -> GridKnowledge r c a
             -> Maybe (GridKnowledge r c a)
inferRows hints
  = ( fmap Vector2
    . sequenceA
    . withStrategy (parTraversable . parTraversable . parTraversable $ rseq)
    . Vector.zipWith inferLine hints
    . Vector2.toVectors
    )


inferColumns :: (KnownNat r, KnownNat c, Eq a)
             =>    Vector c (CappedHints r (Cell a))
                -> GridKnowledge r c a
                -> Maybe (GridKnowledge r c a)
inferColumns hints
  = fmap Vector2.transpose . inferRows hints . Vector2.transpose


inferLine :: (KnownNat lineLen, Eq a)
          =>    CappedHints lineLen (Cell a)
             -> LineKnowledge lineLen a
             -> Maybe (LineKnowledge lineLen a)
inferLine
  = possibleLinesToKnown .: forCapped possibleLines


(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)
infixr 9 .:


possibleLinesToKnown :: (KnownNat n, Eq a)
                     => [Line n a] -> Maybe (LineKnowledge n a)
possibleLinesToKnown
  = sequenceA . fmap allSameToKnown . sequenceA


allSameToKnown :: Eq a => [a] -> Maybe (Knowledge a)
allSameToKnown [] = Nothing
allSameToKnown (x : xs)
  | all (== x) xs  = Just $ Known x
  | otherwise      = Just Unknown


possibleLines :: forall totalHintsLen lineLen a
               . ( KnownNat totalHintsLen
                 , KnownNat lineLen
                 , totalHintsLen <= lineLen
                 , Eq a
                 )
              =>    Hints totalHintsLen (Cell a)
                 -> LineKnowledge lineLen a
                 -> [Line lineLen a]

possibleLines EmptySum line
  = withMatchingHint (Hint @lineLen Empty) line
      (\Nil -> [Vector.replicate' Empty])

possibleLines allHints@(hint :+ hints) line
  = possibleLinesWithHint hint hints line
    <|> if hint ^. value == Empty
          then []
          else switchZero' @ (lineLen - totalHintsLen) []
                 (possibleLinesWithHint (Hint @1 Empty) allHints line)


possibleLinesWithHint :: forall hintLen restHintsLen lineLen a
                       . ( KnownNat lineLen
                         , KnownNat (hintLen + restHintsLen)
                         , (hintLen + restHintsLen) <= lineLen
                         , Eq a
                         )
                      => (  Hint hintLen (Cell a)
                         -> Hints restHintsLen (Cell a)
                         -> LineKnowledge lineLen a
                         -> [Line lineLen a]
                         )

possibleLinesWithHint hint hints line
  = withMatchingHint hint line (prefixHintToSolutions hint hints)
      \\ restHintsLenKnownNat hint hints
  where prefixHintToSolutions h hs rest
          = fmap (Vector.append (Vector.replicate (h ^. run) (h ^. value)))
              (possibleLines hs rest)

restHintsLenKnownNat :: Hint hintLen a -> Hints restLen a -> KnownNat (hintLen + restLen) :- KnownNat restLen
restHintsLenKnownNat (Hint _) _ = Sub Dict


can'tMatch :: Eq a => a -> Knowledge a -> Bool
can'tMatch x = withKnowledge False (/= x)


withMatchingHint :: forall hintLen lineLen f a b
                  . ( Alternative f
                    , hintLen <= lineLen
                    , Eq a
                    )
                 =>    Hint hintLen (Cell a)
                    -> LineKnowledge lineLen a
                    -> (LineKnowledge (lineLen - hintLen) a -> f b)
                    -> f b
withMatchingHint hint line matchCont
  = let (block, rest) = Vector.splitAt (hint ^. run) line
     in if any (can'tMatch (hint ^. value)) block
          then  empty
          else  matchCont rest
