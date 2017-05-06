{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver
                -fplugin TypeNatSolver
  #-}
{-# LANGUAGE DataKinds
           , DeriveFunctor
           , FlexibleContexts
           , GADTs
           , ScopedTypeVariables
           , StandaloneDeriving
           , TupleSections
           , TypeApplications
           , TypeFamilies
           , TypeOperators
  #-}

module Nonosk.PathSolver
  ( Solver
  , solverFromHints
  , updateSolver
  , iterateSolver
  )
where

import Control.Applicative ( Alternative ((<|>), empty)
                           , ZipList (ZipList, getZipList)
                           )

import Control.Lens ( (^.) )

import Data.Bifunctor ( first )

import Data.Coerce ( coerce )

import Data.Constraint ( (:-) (Sub)
                       , Dict (Dict)
                       , (\\)
                       )

import Data.Function ( on )

import Data.List.HT ( takeUntil )

import qualified Data.List.NonEmpty as NonEmpty

import Data.Semigroup ( sconcat )

import Numeric.Natural ( Natural )


import Data.Indexed.Capped ( tryCap
                           , forCapped
                           )

import Data.Indexed.Fin ( Fin
                        , unsafeToFin'
                        )

import Data.Indexed.Index ( Index (Index)
                          , index'
                          , switchZero'
                          )

import Data.Indexed.Nat ( Nat, KnownNat
                        , type (+), type (-)
                        , type (<=)
                        )

import Data.Indexed.Some ( Some (Some)
                         , Some2 (Some2)
                         )

import Data.Indexed.SumList ( SumList ((:+), EmptySum) )
import qualified Data.Indexed.SumList as SumList

import Data.Indexed.Vector ( Vector )
import qualified Data.Indexed.Vector as Vector


import Nonosk.PathTrie ( PathTrie
                       , Link ((:-->))
                       )
import qualified Nonosk.PathTrie as PathTrie

import Nonosk.PosSet ( PosSet, Pos
                     , Direction (RowOrder)
                     )
import qualified Nonosk.PosSet as PosSet

import Nonosk.Hints
import Nonosk.Grid


data Solver :: Direction -> Nat -> Nat -> * -> *
  where Solver :: { rowPaths :: Vector r (PathTrie c (Cell a))
                  , colPaths :: Vector c (PathTrie r (Cell a))
                  , unknownPositions :: PosSet d r c
                  } -> Solver d r c a

deriving instance Functor (Solver d r c)


solverFromHints :: Eq a
                => [[Some Hint a]] -> [[Some Hint a]] -> Maybe (Some2 (Solver d) a)
solverFromHints rowHints colHints
  = case ( (vecSumFromLists $ fmap addSpacers rowHints)
         , (vecSumFromLists $ fmap addSpacers colHints)
         )
      of (Some rows, Some cols)
           -> let rows' = pathsFromUnknownHints rows
                  cols' = pathsFromUnknownHints cols
                  positions = PosSet.allPositions
              in  Some2 <$> (Solver <$> rows' <*> cols'<*> pure positions)


pathsFromUnknownHints :: (KnownNat l, Eq a)
                      =>    Vector n (Some (SumList Hint) (Cell a))
                         -> Maybe (Vector n (PathTrie l (Cell a)))
pathsFromUnknownHints = sequenceA . (fmap . fmap) linePaths . fmap tryCap


-- Data.List.transpose handles ragged lists by giving lists that contain
-- entries from lists that happened to be long enough; we need to a
-- variant that truncates all lists to the length of the smallest
transposeTruncate :: [[a]] -> [[a]]
transposeTruncate = getZipList . sequenceA . coerce


keepKnownPairs :: [(t, Knowledge a)] -> [(t, a)]
keepKnownPairs = foldr checkPair []
  where checkPair (a, Known x) = ((a, x) :)
        checkPair (_, Unknown) = id


columnise :: KnownNat c => [(Pos r c, a)] -> Vector c [(Pos r c, a)]
columnise = foldr insertInColumn (Vector.replicate' [])
  where insertInColumn ::    (Pos n m, a)
                          -> Vector m [(Pos n m, a)]
                          -> Vector m [(Pos n m, a)]
        insertInColumn k@((_, c), _) vs
          = Vector.modifyElem c (k :) vs

_unused = (rowPaths, colPaths)

updateSolver :: forall d r c a
              . (KnownNat r, KnownNat c, Eq a)
             =>    Natural
                -> Solver d r c a
                -> Solver d r c a
updateSolver choiceDepth (Solver rows cols unknowns)
  = Solver rows' cols' unknowns'
  where (prefixes, rows')
           = ( Vector.unzip
             . fmap (PathTrie.checkPrefixesWithinChoiceDepth choiceDepth)
             $ rows
             )

        knowns :: [(Pos r c, Cell a)]
        knowns = ( filter (flip PosSet.member unknowns . fst)
                 . concat
                 . Vector.mapWithIndices (prefixesToKnowns Index)
                 $ prefixes
                 )

        colUpdates :: Vector c [PathTrie r (Cell a) -> PathTrie r (Cell a)]
        colUpdates = (fmap . fmap) updateColumn $ columnise knowns

        cols' = Vector.zipWith (foldr (.) id) colUpdates cols

        unknowns' = unknowns `PosSet.difference` PosSet.fromList (map fst knowns)


prefixesToKnowns :: Eq a => Index c () -> Fin r Int -> [[a]] -> [(Pos r c, a)]
prefixesToKnowns rowLen rowNum rowPrefixes
  = (fmap . first) (rowNum,)
      $ prefixesToKnownPositions rowLen rowPrefixes

prefixesToKnownPositions :: Eq a => Index c () -> [[a]] -> [(Fin c Int, a)]
prefixesToKnownPositions Index
  = ( map (first unsafeToFin')
    . keepKnownPairs
    . zip [0 :: Int ..]
    . fmap (sconcat . fmap Known . NonEmpty.fromList)  -- TODO: avoid partial
    . transposeTruncate
    )

updateColumn :: Eq t => (Pos r c, t) -> PathTrie r t -> PathTrie r t
updateColumn ((r, _), t)
  = PathTrie.prune r (/= t)


updateSolverFromRows :: (KnownNat r, KnownNat c, Eq a)
                     =>    Natural
                        -> Solver RowOrder r c a
                        -> Solver RowOrder r c a
updateSolverFromRows = updateSolver


updateSolverFromColumns :: (KnownNat r, KnownNat c, Eq a)
                        =>    Natural
                           -> Solver RowOrder r c a
                           -> Solver RowOrder r c a
updateSolverFromColumns depth
  = transposeSolver . updateSolver depth . transposeSolver


transposeSolver :: Solver d r c a -> Solver (PosSet.Transpose d) c r a
transposeSolver (Solver rows cols unknowns)
  = Solver cols rows (PosSet.transpose unknowns)


iterateSolver :: forall r c a
               . (KnownNat r, KnownNat c, Eq a)
              => Solver RowOrder r c a -> [Solver RowOrder r c a]
iterateSolver initial
  = let increasingSeries = iterateSolverIncreasingDepth initial
        completeSeries = extendWith iterateSolverMaxDepthUntilStuck
                           increasingSeries
        solved = PosSet.null . unknownPositions
     in takeUntil solved completeSeries


iterateSolverIncreasingDepth
  :: forall r c a
   . (KnownNat r, KnownNat c, Eq a)
  => Solver RowOrder r c a -> [Solver RowOrder r c a]
iterateSolverIncreasingDepth
  = applyUpdates
      [ update depth
      | depth <- [0 .. maxDepth]
      , update <- [updateSolverFromRows, updateSolverFromColumns]
      ]
  where maxDepth = max (index' @ r) (index' @ c)


iterateSolverMaxDepthUntilStuck
  :: forall r c a
   . (KnownNat r, KnownNat c, Eq a)
  => Solver RowOrder r c a -> [Solver RowOrder r c a]
iterateSolverMaxDepthUntilStuck initial
  = let maxDepth = max (index' @ r) (index' @ c)
        series = cycle [ updateSolverFromRows maxDepth
                       , updateSolverFromColumns maxDepth
                       ]
                   `applyUpdates` initial
        stuck = checkAgainstNthPrev 2 False
                  ((==) `on` unknownPositions)
                  series
     in zipWith const series (takeWhile not stuck)


checkAgainstNthPrev :: Int -> Bool -> (a -> a -> Bool) -> [a] -> [Bool]
checkAgainstNthPrev n def p xs
  = let shifted = drop n xs
     in case shifted
          of [] -> const def <$> xs     -- xs not long enough; fill with default
             (_ : _)
                -> replicate n def ++ zipWith p xs shifted

applyUpdates :: [a -> a] -> a -> [a]
applyUpdates = flip (scanl (flip ($)))


extendWith :: (a -> [a]) -> [a] -> [a]
extendWith _ [] = []
extendWith f (x : xs) = go x xs
  where go prev [] = f prev
        go prev (y : ys) = prev : go y ys



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


linePaths :: (KnownNat lineLen, Eq a)
          =>    CappedHints lineLen (Cell a)
             -> PathTrie lineLen (Cell a)
linePaths = forCapped pathsFromHints


pathsFromHints :: forall lineLen hintsLen a
                . ( KnownNat hintsLen
                  , KnownNat lineLen
                  , hintsLen <= lineLen
                  , Eq a
                  )
               =>    Hints hintsLen (Cell a)
                  -> PathTrie lineLen (Cell a)
pathsFromHints EmptySum = pure Empty
pathsFromHints allHints@(hint :+ restHints)
  = pathsFollowingHint hint restHints
    <|> switchZero' @ (lineLen - hintsLen)
          empty
          ( case hint ^. value
              of Empty -> empty
                 _     -> PathTrie.Fork [Empty :--> pathsFromHints allHints]
          )


pathsFollowingHint :: forall hintLen restHintsLen lineLen a
                    . ( KnownNat lineLen
                      , KnownNat (hintLen + restHintsLen)
                      , (hintLen + restHintsLen) <= lineLen
                      , Eq a
                      )
                   => (  Hint hintLen (Cell a)
                      -> Hints restHintsLen (Cell a)
                      -> PathTrie lineLen (Cell a)
                      )
pathsFollowingHint hint hints
  = PathTrie.repeatAndThen (hint ^. run) (hint ^. value)
      $ pathsFromHints @ (lineLen - hintLen) hints
          \\ restHintsLenKnownNat hint hints


restHintsLenKnownNat :: Hint hintLen a -> Hints restLen a -> KnownNat (hintLen + restLen) :- KnownNat restLen
restHintsLenKnownNat (Hint _) _ = Sub Dict
