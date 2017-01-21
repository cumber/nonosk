{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise
                -fplugin GHC.TypeLits.KnownNat.Solver
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

import Control.Lens ((^.))
import qualified Control.Lens as Lens

import Data.Sized.Builtin (Sized, pattern (:<))
import qualified Data.Sized.Builtin as Sized

import qualified Data.Type.Natural.Class.Order as Nat

import Data.Singletons (SomeSing, toSing, withSomeSing)
import Data.Singletons.Prelude ((%:-), (:<=), (%:<=), Sing(STrue, SFalse))
import Data.Singletons.TypeLits
  ( KnownNat
  , Nat
  , Sing(SNat)
  , SNat
  , withKnownNat
  )

import GHC.Prim (Proxy#, proxy#)
import GHC.TypeLits (type (+), type (-), natVal')

import Numeric.Natural (Natural)

import Proof.Equational ((:~:))
import qualified Proof.Equational as Proof
import qualified Proof.Propositional as Proof


-- | A Hint identifies a run of cells filled with a constant value.
data Hint n a = Hint { _value :: !a, _run :: !(SNat n) }
  deriving (Functor)
Lens.makeLenses ''Hint


data Hints n a
  where None :: Hints 0 a
        Cons :: (  Hint blockLen a
                -> Hints restLen a
                -> Hints (blockLen + restLen) a
                )
infixr 5 `Cons`
deriving instance Functor (Hints n)


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


-- | A List is a 'Sized' wrapper around ordinary lists
type List n a = Sized [] n a

-- | A Line is a 'List' of 'Cell's
type Line n a = List n (Cell a)

-- | A LineKnowledge is a 'List' of 'Knowledge' 'Cell's
type LineKnowledge n a = List n (Knowledge (Cell a))

-- | A Line' is a 'List' of 'Cell''s
type Line' n = List n Cell'

-- | A @LineKnowledge' is a 'List' of @'Knowledge' 'Cell''@s
type LineKnowledge' n = List n (Knowledge Cell')


newtype Grid (r :: Nat) (c :: Nat) (a :: *)
  = Grid { unGrid :: List r (List c a) }
  deriving (Eq, Functor, Show)


data Puzzle :: (* -> *) -> * -> *
  where Puzzle     :: { grid :: Grid (r :: Nat) (c :: Nat) (f a)
                      --, rowHints :: List r (Hint broken (Cell a))
                      --, colHints :: List c (Hint broken (Cell a))
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
toRawLists = Sized.toList . Sized.map Sized.toList . unGrid


constGrid :: SNat r -> SNat c -> a -> Grid r c a
constGrid r c = Grid . Sized.replicate r . Sized.replicate c


{-
dropSize :: Puzzle f a -> Puzzle' f a
dropSize (Puzzle { grid = g, rowHints = rhs, colHints = chs })
  = Puzzle' { grid' = toRawLists g
            , rowHints' = Sized.toList rhs
            , colHints' = Sized.toList chs
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
       . Eq a
      =>    SNat totalHintsLen
         -> SNat lineLen
         -> Hints totalHintsLen (Cell a)
         -> LineKnowledge lineLen a
         -> [Line lineLen a]

infer _ sLineLen None line
  = case matchHint (Hint Empty sLineLen) line
      of Just Sized.NilL -> [Sized.replicate sLineLen Empty]
         Nothing -> []

infer sTotalHintsLen sLineLen ((hint@(Hint _ sBlockLen@SNat) :: Hint blockLen (Cell a)) `Cons` hints) line
  = let sRestLen = sLineLen %:- sBlockLen
        sHintsLen = sTotalHintsLen %:- sBlockLen
        hints' :: Hints (totalHintsLen - blockLen) (Cell a)
        hints' = hints
     in case sTotalHintsLen %:<= sLineLen
          of STrue
               -> let proof = prove sBlockLen sHintsLen sTotalHintsLen sRestLen sLineLen Proof.Refl Proof.Refl Proof.Witness
                   in ( withWitness2 proof
                      $ case matchHint hint line
                          of Nothing -> []
                             Just rest
                               -> Sized.append (Sized.replicate (hint ^. run) (hint ^. value))
                                    <$> infer sHintsLen sRestLen hints' rest
                      )

withWitness2 :: (Proof.IsTrue a, Proof.IsTrue b) -> ((a ~ True, b ~ True) => r) -> r
withWitness2 (a, b) = Proof.withWitness a . Proof.withWitness b

prove
  :: SNat a -> SNat b -> SNat c -> SNat d -> SNat e
      -> (a + b :~: c) -> (a + d :~: e) -> Proof.IsTrue (c :<= e)
      -> (Proof.IsTrue (a :<= e), Proof.IsTrue (b :<= d))
prove a b _ d _ Proof.Refl Proof.Refl Proof.Witness
  = Proof.withWitness (Nat.plusLeqL a d)
      ( Proof.withWitness (Nat.plusCancelLeqL a b d Proof.Witness)
        (Proof.Witness, Proof.Witness)
      )

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
  = let (block, rest) = Sized.splitAt (hint ^. run) line
    in  if any (can'tMatch (hint ^. value)) block
          then  Nothing
          else  Just rest


natVal :: forall n. KnownNat n => Natural
natVal = fromInteger $ natVal' (proxy# :: Proxy# n)


natSing :: Natural -> SomeSing Nat
natSing = toSing . fromIntegral


withSomeNat :: Natural -> (forall n. SNat n -> r) -> r
withSomeNat n r = withSomeSing (fromIntegral n) (\s -> withKnownNat s (r s))
