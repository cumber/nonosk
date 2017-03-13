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

import Control.Lens ((^.))
import qualified Control.Lens as Lens

import Data.Constraint ( (:-) (Sub)
                       , Dict (Dict)
                       , (\\)
                       )

import Data.Foldable ( toList )

import GHC.TypeLits ( Nat
                    , KnownNat
                    , type (+)
                    , type (-)
                    , type (<=)
                    )


import Data.Indexed.Index ( Index (Index)
                          , switchZero
                          )

import Data.Indexed.SumList

import Data.Indexed.Vector ( Vector (Nil) )
import qualified Data.Indexed.Vector as Vector


-- | A Hint identifies a run of cells filled with a constant value.
data Hint n a = Hint { _value :: !a, _run :: !(Index n ()) }
  deriving (Show, Functor)
Lens.makeLenses ''Hint


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


data Puzzle :: (* -> *) -> * -> *
  where Puzzle     :: { grid :: Grid (r :: Nat) (c :: Nat) (f a)
                      --, rowHints :: Vector r (Hint broken (Cell a))
                      --, colHints :: Vector c (Hint broken (Cell a))
                      } -> Puzzle f a

deriving instance Functor f => Functor (Puzzle f)
deriving instance (Show a, Show (f a)) => Show (Puzzle f a)


-- | A Puzzle' is a non-dependently-typed analogue of 'Puzzle', with lists
--   of rows cells not constrained to form a proper grid, and not constrained
--   to match the number of row and column hints.
data Puzzle' f broken a
  = Puzzle' { grid' :: [[f a]]
            --, rowHints' :: [Hint broken (Cell a)]
            --, colHints' :: [Hint broken (Cell a)]
            }
  deriving (Eq, Show)


toRawLists :: Grid r c a -> [[a]]
toRawLists = toList . fmap toList . unGrid


constGrid :: Index r () -> Index c () -> a -> Grid r c a
constGrid r c = Grid . Vector.replicate r . Vector.replicate c


{-
dropSize :: Puzzle f a -> Puzzle' f a
dropSize (Puzzle { grid = g, rowHints = rhs, colHints = chs })
  = Puzzle' { grid' = toRawLists g
            , rowHints' = toList rhs
            , colHints' = toList chs
            }


instance (Eq (f a), Eq a) => Eq (Puzzle f a)
  where (==) = (==) `on` dropSize


initPuzzle :: [[Hint a]] -> [[Hint a]] -> Puzzle Cell a
initPuzzle extRowHints extColHints
  = let nRows = fromIntegral $ length extRowHints
        nCols = fromIntegral $ length extColHints
    in  case (toSing nRows, toSing nCols) of
          (SomeSing r, SomeSing c) -> Puzzle (constGrid r c Empty) _ _
-}


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
  = withMatchingHint (Hint Empty (Index @ lineLen)) line
      (\Nil -> [Vector.replicate' Empty])

possibleLines allHints@(hint :+ hints) line
  = possibleLinesWithHint hint hints line
    <|> if hint ^. value == Empty
          then []
          else switchZero (Index @ (lineLen - totalHintsLen)) []
                 (possibleLinesWithHint (Hint Empty (Index @ 1)) allHints line)


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
restHintsLenKnownNat (Hint _ Index) _ = Sub Dict


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
