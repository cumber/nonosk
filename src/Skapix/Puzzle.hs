{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver
                -fplugin TypeNatSolver
  #-}
{-# LANGUAGE DataKinds
           , DeriveFunctor
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

import Data.Constraint ( (:-) (Sub)
                       , Dict (Dict)
                       , (\\)
                       , (:=>) (ins)
                       )

import Data.Foldable ( toList )

import GHC.Natural ( Natural )

import GHC.TypeLits ( Nat
                    , KnownNat
                    , type (+)
                    , type (-)
                    , type (<=)
                    )


import Data.Indexed.Capped ( Capped
                           , tryCap
                           )

import Data.Indexed.Index ( Index (Index)
                          , index'
                          , someIndex
                          , switchZero
                          )

import Data.Indexed.Some ( Some (Some)
                         , Some2 (Some2)
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

instance Show a => Show (Hint n a)
  where showsPrec p (Hint x)
          = showParen (p > appPrec)
              ( showString "Hint @ "
              . showsPrec (appPrec + 1) (index' @ n)
              . showString " "
              . showsPrec (appPrec + 1) x
              )
          where appPrec = 10

instance Show a => KnownNat n :=> Show (Hint n a)
  where ins = Sub Dict


makeHint :: Index n () -> a -> Hint n a
makeHint Index = Hint

value :: Lens (Hint n a) (Hint n b) a b
value f (Hint x) = fmap Hint (f x)

run :: Lens (Hint n a) (Hint m a) (Index n ()) (Index m ())
run f (Hint x) = flip makeHint x <$> f Index

someHint :: Natural -> a -> Some Hint a
someHint n a = withSome (someIndex n) (Some . flip makeHint a)


type Hints sum a = SumList Hint sum a


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
  deriving (Eq, Functor, Show)


data Puzzle :: Nat -> Nat -> * -> *
  where Puzzle     :: { grid :: Grid (r :: Nat) (c :: Nat) (Knowledge a)
                      , rowHints :: Vector r (Capped c (SumList Hint) a)
                      , colHints :: Vector c (Capped r (SumList Hint) a)
                      } -> Puzzle r c a

deriving instance Functor (Puzzle r c)
deriving instance (KnownNat r, KnownNat c, Show a) => Show (Puzzle r c a)

instance Show a => (KnownNat r, KnownNat c) :=> Show (Puzzle r c a)
  where ins = Sub Dict


toRawLists :: Grid r c a -> [[a]]
toRawLists = toList . fmap toList . unGrid


constGrid :: Index r () -> Index c () -> a -> Grid r c a
constGrid Index Index = constGrid'


constGrid' :: (KnownNat r, KnownNat c) => a -> Grid r c a
constGrid' = Grid . Vector.replicate' . Vector.replicate'


initPuzzle :: [[Some Hint a]] -> [[Some Hint a]] -> Maybe (Some2 Puzzle a)
initPuzzle extRowHints extColHints
  = case ((vecSumFromLists extRowHints), (vecSumFromLists extColHints))
      of (Some rows, Some cols)
           -> let rows' = sequenceA $ fmap tryCap rows
                  cols' = sequenceA $ fmap tryCap cols
              in  Some2 <$> (Puzzle <$> pure (constGrid' Unknown)
                                    <*> rows'
                                    <*> cols'
                            )



vecSumFromLists :: [[Some f a]] -> Some Vector (Some (SumList f) a)
vecSumFromLists = Vector.fromList . fmap SumList.fromSomeList


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
