{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise
                -fplugin GHC.TypeLits.KnownNat.Solver
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

import Control.Applicative (Alternative((<|>), empty))

import Control.Lens ((^.))
import qualified Control.Lens as Lens

import Data.Constraint
import Data.Constraint.Nat

import Data.Semigroup ((<>))

import Data.Singletons.TypeLits.Show ()

import Data.Singletons (SomeSing, toSing, withSomeSing)
import Data.Singletons.Prelude ((%:-), (%:+), (:<), (:<=))
import Data.Singletons.TypeLits
  ( Nat
  , Sing(SNat)
  , SNat
  , withKnownNat
  )
import qualified Data.Singletons.TypeLits as Sing
import qualified Data.Singletons.Prelude as Sing

import GHC.TypeLits (type (+), type (-), type (<=))

import Numeric.Natural (Natural)

import Proof.Equational ((:~:))
import qualified Proof.Equational as Proof
import qualified Proof.Propositional as Proof

import Data.Indexed.Vector ( Vector ((:^), Nil) )
import qualified Data.Indexed.Vector as Vector

import Data.Indexed.Some


-- | A Hint identifies a run of cells filled with a constant value.
data Hint n a = Hint { _value :: !a, _run :: !(SNat n) }
  deriving (Show, Functor)
Lens.makeLenses ''Hint


data Hints n a
  where None :: Hints 0 a
        Cons :: (  Hint blockLen a
                -> Hints restLen a
                -> Hints (blockLen + restLen) a
                )
infixr 5 `Cons`
deriving instance Functor (Hints n)
deriving instance Show a => Show (Hints n a)


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


constGrid :: SNat r -> SNat c -> a -> Grid r c a
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


infer :: forall totalHintsLen lineLen a
       . ( Sing.KnownNat totalHintsLen
         , Sing.KnownNat lineLen
         , totalHintsLen <= lineLen
         , Eq a
         )
      =>    Hints totalHintsLen (Cell a)
         -> LineKnowledge lineLen a
         -> [Line lineLen a]

infer None line
  = maybe [] (\Nil -> [Vector.replicate' Empty])
      (matchHint (Hint Empty (SNat @ lineLen)) line)

infer ((hint :: Hint hintLen (Cell a)) `Cons` (hints :: Hints remainingHintsLen (Cell a))) line
  = withMatchingHint hint line (\rest -> (Vector.append $ Vector.replicate (hint ^. run) (hint ^. value)) <$> infer hints rest)
      \\ leTrans @ hintLen @ totalHintsLen @ lineLen
      -- \\ plusMonotone1 


withWitness2 :: (Proof.IsTrue a, Proof.IsTrue b) -> ((a ~ True, b ~ True) => r) -> r
withWitness2 (a, b) = Proof.withWitness a . Proof.withWitness b


can'tMatch :: Eq a => a -> Knowledge a -> Bool
can'tMatch x = withKnowledge False (/= x)


matchHint :: forall l n a
           . ( (l :<= n) ~ True
             , Eq a
             )
          =>    Hint l (Cell a)
             -> LineKnowledge n a
             -> Maybe (LineKnowledge (n - l) a)
matchHint hint line
  = let (block, rest) = Vector.splitAt (hint ^. run) line
    in  if any (can'tMatch (hint ^. value)) block
          then  Nothing
          else  Just rest


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


natVal :: SNat n -> Natural
natVal n@SNat = fromInteger $ Sing.natVal n


natSing :: Natural -> SomeSing Nat
natSing = toSing . fromIntegral


withSomeNat :: Natural -> (forall n. SNat n -> r) -> r
withSomeNat n r = withSomeSing (fromIntegral n) (\s -> withKnownNat s (r s))
