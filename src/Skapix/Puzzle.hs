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

module Skapix.Puzzle
where

import Control.Applicative ( Alternative ((<|>), empty) )

import Control.Lens ( Lens
                    , (^.)
                    )
import qualified Control.Lens as Lens

import Control.Monad ( (<=<) )

import Data.Constraint ( (:-) (Sub)
                       , Dict (Dict)
                       , (\\)
                       )

import Data.Foldable ( toList )

import Data.List ( unfoldr )

import GHC.Natural ( Natural )


import Data.Indexed.Capped ( Capped
                           , tryCap
                           , forCapped
                           )

import Data.Indexed.ForAnyKnownIndex ( ForAnyKnownIndex (instAnyKnownIndex)
                                     , ForAnyKnownIndexF (instAnyKnownIndexF)
                                     , ForAnyKnownIndex2 (instAnyKnownIndex2)
                                     )

import Data.Indexed.Index ( Index (Index)
                          , index'
                          , someIndex
                          , switchZero
                          )

import Data.Indexed.Nat ( Nat, KnownNat
                        , type (+), type (-)
                        , type (<=)
                        )

import Data.Indexed.Some ( Some (Some)
                         , Some2 (Some2)
                         , forSome
                         , withSome
                         )

import Data.Indexed.SumList ( SumList ((:+), EmptySum) )
import qualified Data.Indexed.SumList as SumList

import Data.Indexed.Vector ( Vector (Nil)
                           )
import qualified Data.Indexed.Vector as Vector


-- | A Hint identifies a run of cells filled with a constant value.
data Hint n a
  where Hint :: KnownNat n => !a -> Hint n a

deriving instance Eq a => Eq (Hint n a)


deriving instance Functor (Hint n)

instance ForAnyKnownIndexF Functor Hint
  where instAnyKnownIndexF = Sub Dict


instance Show a => Show (Hint n a)
  where showsPrec p (Hint x)
          = showParen (p > appPrec)
              ( showString "Hint @ "
              . showsPrec (appPrec + 1) (index' @ n)
              . showString " "
              . showsPrec (appPrec + 1) x
              )
          where appPrec = 10

instance Show a => ForAnyKnownIndex Show Hint a
  where instAnyKnownIndex = Sub Dict


makeHint :: Index n () -> a -> Hint n a
makeHint Index = Hint

value :: Lens (Hint n a) (Hint n b) a b
value f (Hint x) = fmap Hint (f x)

run :: Lens (Hint n a) (Hint m a) (Index n ()) (Index m ())
run f (Hint x) = flip makeHint x <$> f Index

someHint :: Natural -> a -> Some Hint a
someHint n a = withSome (someIndex n) (Some . flip makeHint a)


type Hints sum a = SumList Hint sum a
type CappedHints length a = Capped length (SumList Hint) a


-- | A Cell can be empty or filled with a particular value.
data Cell a = Empty
            | Filled !a
  deriving (Eq, Ord, Functor, Show)
Lens.makePrisms ''Cell

-- | Eliminator for 'Cell'
withCell :: r -> (a -> r) -> Cell a -> r
withCell r _ Empty = r
withCell _ f (Filled v) = f v

-- | Simpler 'Cell' for standard 2-colour pixel puzzles, which don't need to
--   distinguish which value is filled since there's only one.
type Cell' = Cell ()

-- | A Knowledge can be either unknown, or known to be a particular value.
data Knowledge a = Unknown
                 | Known !a
  deriving (Eq, Ord, Functor, Show)
Lens.makePrisms ''Knowledge

-- | Elminator for 'Knowledge'
withKnowledge :: r -> (a -> r) -> Knowledge a -> r
withKnowledge r _ Unknown = r
withKnowledge _ f (Known v) = f v

-- | Witnesses that @'Cell' a@ is isomphic to @'Maybe' a@
maybeCell :: Lens.Iso' (Maybe a) (Cell a)
maybeCell = Lens.iso forward backward
  where forward = maybe Empty Filled
        backward = withCell Nothing Just

-- | Witnesses that @'Knowledge' a@ is isomorphic to @'Maybe' a@
maybeKnowledge :: Lens.Iso' (Maybe a) (Knowledge a)
maybeKnowledge = Lens.iso forward backward
  where forward = maybe Unknown Known
        backward = withKnowledge Nothing Just


-- | A Line is a 'Vector' of 'Cell's
type Line n a = Vector n (Cell a)

-- | A LineKnowledge is a 'Vector' of 'Knowledge' 'Cell's
type LineKnowledge n a = Vector n (Knowledge (Cell a))

-- | A Line' is a 'Vector' of 'Cell''s
type Line' n = Vector n Cell'

-- | A @LineKnowledge' is a 'Vector' of @'Knowledge' 'Cell''@s
type LineKnowledge' n = Vector n (Knowledge Cell')


newtype Grid (r :: Nat) (c :: Nat) (a :: *)
  = Grid { unGrid :: Vector r (Vector c a) }
  deriving (Eq, Show, Functor, Foldable, Traversable)


instance Show a => ForAnyKnownIndex2 Show Grid a
  where instAnyKnownIndex2 = Sub Dict


cellToAscii :: Knowledge (Cell ()) -> Char
cellToAscii (Known Empty) = ' '
cellToAscii (Known (Filled ())) = '#'
cellToAscii Unknown = '?'

asciiToCell :: Char -> Maybe (Knowledge (Cell ()))
asciiToCell ' ' = Just $ Known Empty
asciiToCell '#' = Just $ Known (Filled ())
asciiToCell '?' = Just Unknown
asciiToCell _ = Nothing

gridToAscii :: Grid r c (Knowledge (Cell ())) -> String
gridToAscii = unlines . gridToAsciiLines

gridToAsciiLines :: Grid r c (Knowledge (Cell ())) -> [String]
gridToAsciiLines = toRawLists . fmap cellToAscii

asciiToGrid :: String -> Maybe (Some2 Grid (Knowledge (Cell ())))
asciiToGrid = asciiLinesToGrid . lines

asciiLinesToGrid :: [String] -> Maybe (Some2 Grid (Knowledge (Cell ())))
asciiLinesToGrid = fromRawLists <=< sequenceA2 . (fmap . fmap) asciiToCell


sequenceA2 :: (Traversable s, Traversable t, Applicative f)
           => s (t (f a)) -> f (s (t a))
sequenceA2 = sequenceA . fmap sequenceA


data Puzzle :: Nat -> Nat -> * -> *
  where Puzzle     :: { grid :: Grid (r :: Nat) (c :: Nat) (Knowledge (Cell a))
                      , rowHints :: Vector r (CappedHints c (Cell a))
                      , colHints :: Vector c (CappedHints r (Cell a))
                      } -> Puzzle r c a

deriving instance Functor (Puzzle r c)
deriving instance (KnownNat r, KnownNat c, Show a) => Show (Puzzle r c a)

instance Show a => ForAnyKnownIndex2 Show Puzzle a
  where instAnyKnownIndex2 = Sub Dict


toRawLists :: Grid r c a -> [[a]]
toRawLists = toList . fmap toList . unGrid


fromRawLists :: [[a]] -> Maybe (Some2 Grid a)
fromRawLists [] = Just . Some2 . Grid @ 0 @ 0 $ Nil
fromRawLists (row : rows)
  = withSome (Vector.fromList row) (rowsSameLength rows)
  where rowsSameLength :: forall n a
                        . KnownNat n
                       => [[a]] -> Vector n a -> Maybe (Some2 Grid a)
        rowsSameLength xss initial
          = let maybeRest = sequenceA $ Vector.fromListIndexed' @ n <$> xss
                maybeRows = Vector.fromList . (initial :) <$> maybeRest
             in forSome (Some2 . Grid) <$> maybeRows


constGrid :: Index r () -> Index c () -> a -> Grid r c a
constGrid Index Index = constGrid'


constGrid' :: (KnownNat r, KnownNat c) => a -> Grid r c a
constGrid' = Grid . Vector.replicate' . Vector.replicate'


transpose :: KnownNat c => Grid r c a -> Grid c r a
transpose = Grid . Vector.transpose . unGrid


initPuzzle :: Eq a => [[Some Hint a]] -> [[Some Hint a]] -> Maybe (Some2 Puzzle a)
initPuzzle extRowHints extColHints
  = case ( (vecSumFromLists $ fmap addSpacers extRowHints)
         , (vecSumFromLists $ fmap addSpacers extColHints)
         )
      of (Some rows, Some cols)
           -> let rows' = sequenceA $ fmap tryCap rows
                  cols' = sequenceA $ fmap tryCap cols
              in  Some2 <$> (Puzzle <$> pure (constGrid' Unknown)
                                    <*> rows'
                                    <*> cols'
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
            => Puzzle r c a -> Maybe (Grid r c (Cell a))
solvePuzzle puzzle
  = let finalGrid = safeLast . inferGrid (rowHints puzzle) (colHints puzzle)
                             $ (grid puzzle)
        checkSolved = sequenceA . fmap (withKnowledge Nothing Just)
     in checkSolved =<< finalGrid


safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x : xs) = Just $ go x xs
  where go y [] = y
        go _ (y : ys) = go y ys

inferGrid :: (KnownNat r, KnownNat c, Eq a)
            =>    Vector r (CappedHints c (Cell a))
               -> Vector c (CappedHints r (Cell a))
               -> Grid r c (Knowledge (Cell a)) -> [Grid r c (Knowledge (Cell a))]
inferGrid rHints cHints grd
  = unfoldr step (inferRows rHints, inferColumns cHints, grd)
  where step :: Eq a
             =>    (a -> Maybe a, a -> Maybe a, a)
                -> Maybe (a, (a -> Maybe a, a -> Maybe a, a))
        step (f, g, x) = dup3rd <$> (checkNotEq x (g, f, ) =<< f x)

        checkNotEq x f x'
          | x == x'   = Nothing
          | otherwise = Just (f x')

        dup3rd (x, y, z) = (z, (x, y, z))



iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = unfoldr (fmap dup . f)
  where dup x = (x, x)


inferRows :: (KnownNat c, Eq a)
          =>    Vector r (CappedHints c (Cell a))
             -> Grid r c (Knowledge (Cell a))
             -> Maybe (Grid r c (Knowledge (Cell a)))
inferRows hints
  = fmap Grid . sequenceA . Vector.zipWith inferLine hints . unGrid


inferColumns :: (KnownNat r, KnownNat c, Eq a)
             =>    Vector c (CappedHints r (Cell a))
                -> Grid r c (Knowledge (Cell a))
                -> Maybe (Grid r c (Knowledge (Cell a)))
inferColumns hints = fmap transpose . inferRows hints . transpose


inferLine :: (KnownNat lineLen, Eq a)
          =>    CappedHints lineLen (Cell a)
             -> LineKnowledge lineLen a
             -> Maybe (LineKnowledge lineLen a)
inferLine = possibleLinesToKnown .: forCapped possibleLines


(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = (.) . (.)


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
          else switchZero (Index @ (lineLen - totalHintsLen)) []
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
