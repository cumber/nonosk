{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver
                -fplugin TypeNatSolver
  #-}
{-# LANGUAGE DataKinds
           , DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , KindSignatures
           , MagicHash
           , MultiParamTypeClasses
           , PatternSynonyms
           , PolyKinds
           , RankNTypes
           , ScopedTypeVariables
           , StandaloneDeriving
           , TemplateHaskell
           , TupleSections
           , TypeApplications
           , TypeFamilies
           , TypeOperators
           , AllowAmbiguousTypes
  #-}

module Nonosk.Puzzle
  ( Puzzle
  , initPuzzle
  , makePuzzle
  , solvePuzzle
  , solvePuzzleSteps
  , inferGrid
  , inferLine
  , possibleLines
  )
where

import Control.Applicative ( Alternative ((<|>), empty) )

import Control.Lens ( (^.) )
import qualified Control.Lens as Lens

import Control.Monad ( (<=<) )

import Control.Parallel.Strategies ( parTraversable
                                   , rseq
                                   , withStrategy
                                   )

import Data.Constraint ( (:-) (Sub)
                       , Dict (Dict)
                       , (\\)
                       )

import Data.Foldable ( foldl' )

import Data.Function.Memoize ( memoFix2 )

import Data.List ( unfoldr )


import Data.Indexed.Capped ( tryCap
                           , forCapped
                           )

import Data.Indexed.ForAnyKnownIndex ( ForAnyKnownIndex2 (instAnyKnownIndex2) )

import Data.Indexed.Index ( index
                          , index'
                          , switchZero'
                          )

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


data LineSpec lineLen a
  = LineSpec { _lineHints :: CappedHints lineLen (Cell a)
             , _lineChoices :: Integer
             }
  deriving (Functor, Show)
Lens.makeLenses ''LineSpec

data Puzzle :: Nat -> Nat -> * -> *
  where Puzzle :: { rowHints :: Vector r (CappedHints c (Cell a))
                  , colHints :: Vector c (CappedHints r (Cell a))
                  , puzzleGrid :: GridKnowledge r c a
                  } -> Puzzle r c a

deriving instance Functor (Puzzle r c)
deriving instance (KnownNat r, KnownNat c, Show a) => Show (Puzzle r c a)

instance Show a => ForAnyKnownIndex2 Show Puzzle a
  where instAnyKnownIndex2 = Sub Dict


initPuzzle :: Eq a => [[Some Hint a]] -> [[Some Hint a]] -> Maybe (Some2 Puzzle a)
initPuzzle extRowHints extColHints
  = case ( (vecSumFromLists $ fmap addSpacers extRowHints)
         , (vecSumFromLists $ fmap addSpacers extColHints)
         )
      of (Some rows, Some cols)
           -> let rows' = sequenceA $ fmap tryCap rows
                  cols' = sequenceA $ fmap tryCap cols
              in  Some2 <$> (Puzzle <$> rows'
                                    <*> cols'
                                    <*> pure (Vector2.replicate' Unknown)
                            )


makePuzzle :: Eq a
           =>     [[Some Hint a]]
               -> [[Some Hint a]]
               -> Some2 Vector2 (Knowledge (Cell a))
               -> Maybe (Some2 Puzzle a)
makePuzzle extRowHints extColHints extGrid
  = case ( (vecSumFromLists $ fmap addSpacers extRowHints)
         , (vecSumFromLists $ fmap addSpacers extColHints)
         )
      of (Some rows, Some cols)
           -> let rows' = sequenceA $ fmap tryCap rows
                  cols' = sequenceA $ fmap tryCap cols
              in  Some2 <$> (Puzzle <$> rows'
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


solvePuzzle :: (KnownNat r, KnownNat c, Eq a)
            => Puzzle r c a -> Maybe (Grid r c a)
solvePuzzle puzzle
  = let finalGrid = safeLast . inferGrid (rowSpecs puzzle) (colSpecs puzzle)
                             $ (puzzleGrid puzzle)
        checkSolved = sequenceA . fmap (withKnowledge Nothing Just)
     in checkSolved =<< finalGrid


safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x : xs) = Just $ go x xs
  where go y [] = y
        go _ (y : ys) = go y ys


solvePuzzleSteps :: (KnownNat r, KnownNat c, Eq a)
                 => Puzzle r c a -> [GridKnowledge r c a]
solvePuzzleSteps p
  = inferGrid (rowSpecs p) (colSpecs p) (puzzleGrid p)


inferGrid :: (KnownNat r, KnownNat c, Eq a)
            =>    Vector r (LineSpec c a)
               -> Vector c (LineSpec r a)
               -> GridKnowledge r c a -> [GridKnowledge r c a]
inferGrid rSpecs cSpecs
  = raiseThresholdAfterDup (iterateMaybe . doStep) 2000
  where doStep t = inferRows rSpecs t <=< inferColumns cSpecs t

        raiseThresholdAfterDup f threshold grid
          = foldr findDup [] (f threshold grid)
            where findDup g gs
                    = g : case gs
                            of [] -> []
                               (g' : _)
                                 | g == g'    -> raiseOrStop g
                                 | otherwise  -> gs
                  raiseOrStop g
                    | threshold > maxLinePriority rSpecs cSpecs g  = []
                    | otherwise = raiseThresholdAfterDup f (threshold * 2) g


maxLinePriority rSpecs cSpecs grid
  = max (maxRowPriority rSpecs $ Vector2.toVectors grid)
        (maxRowPriority cSpecs . Vector2.toVectors . Vector2.transpose $ grid)
  where maxRowPriority
          = maximum .: Vector.zipWith linePriority . fmap _lineChoices


iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = unfoldr (fmap dup . f)
  where dup x = (x, x)


inferRows :: (KnownNat c, Eq a)
          =>    Vector r (LineSpec c a)
             -> Integer
             -> GridKnowledge r c a
             -> Maybe (GridKnowledge r c a)
inferRows hints threshold
  = ( fmap Vector2
    . sequenceA
    . withStrategy (parTraversable . parTraversable . parTraversable $ rseq)
    . Vector.zipWith (inferLine threshold) hints
    . Vector2.toVectors
    )


inferColumns :: (KnownNat r, KnownNat c, Eq a)
             =>    Vector c (LineSpec r a)
                -> Integer
                -> GridKnowledge r c a
                -> Maybe (GridKnowledge r c a)
inferColumns hints threshold
  = fmap Vector2.transpose . inferRows hints threshold . Vector2.transpose


inferLine :: (KnownNat lineLen, Eq a)
          =>    Integer
             -> LineSpec lineLen a
             -> LineKnowledge lineLen a
             -> Maybe (LineKnowledge lineLen a)
inferLine threshold spec line
  | linePriority (spec ^. lineChoices) line > threshold = Just line
  | otherwise = go (spec ^. lineHints) line
  where go :: (KnownNat l, Eq a)
           => CappedHints l (Cell a) -> LineKnowledge l a
                -> Maybe (LineKnowledge l a)
        go = possibleLinesToKnown .: forCapped possibleLines


countKnown :: LineKnowledge lineLen a -> Integer
countKnown = foldl' (\count -> (count +) . withKnowledge 0 (const 1)) 0


linePriority :: Integer -> LineKnowledge lineLen a -> Integer
linePriority numChoices line
  = numChoices `div` (1 + countKnown line)


(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)
infixr 9 .:


makeLineSpec :: forall lineLen a
              . KnownNat lineLen
             => CappedHints lineLen (Cell a) -> LineSpec lineLen a
makeLineSpec hints
  = LineSpec { _lineChoices
                 = forCapped ( numPossibleLines (toInteger $ index' @ lineLen)
                             . SumList.toListWith hintLen
                             )
                     hints
             , _lineHints = hints
             }
  where hintLen = toInteger . index . Lens.view run

numPossibleLines :: Integer -> [Integer] -> Integer
numPossibleLines = memoFix2 go
  where go _ remainingLineLen _
          | remainingLineLen < 0  = 0
        go _ _ [] = 1
        go f remainingLineLen allHs@(h : hs)
          = ( f (remainingLineLen - 1) allHs   -- place a blank here
            + f (remainingLineLen - h) hs      -- place the hint here
            )


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
