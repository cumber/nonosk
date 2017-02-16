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

import Control.Applicative (Alternative((<|>), empty))

import Control.Lens ((^.))
import qualified Control.Lens as Lens

import Data.Sized.Builtin (Sized, pattern (:<))
import qualified Data.Sized.Builtin as Sized

import Data.Type.Natural.Class.Order (DiffNat(DiffNat))
import qualified Data.Type.Natural.Class.Order as Nat
import qualified Data.Type.Natural.Class.Arithmetic as Nat

import Data.Type.Ordinal.Builtin (Ordinal)
import qualified Data.Type.Ordinal.Builtin as Ord

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

import GHC.TypeLits (type (+), type (-))

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
       . ((totalHintsLen :<= lineLen) ~ True, Eq a)
      =>    SNat totalHintsLen
         -> SNat lineLen
         -> Hints totalHintsLen (Cell a)
         -> LineKnowledge lineLen a
         -> [Line lineLen a]

infer _ sLineLen None line
  = case matchHint (Hint Empty sLineLen) line
      of Just Sized.NilL -> [Sized.replicate sLineLen Empty]
         Nothing -> []

infer sTotalHintsLen sLineLen (hint `Cons` (hints :: Hints restHintsLen (Cell a))) line
  = case Nat.leqWitness sTotalHintsLen sLineLen Proof.Witness
      of DiffNat _ sMaxSpaceLen
           -> do  Ord.OLt sSpaceLen <- Ord.enumOrdinal $ sMaxSpaceLen %:+ (SNat @ 1)
                  let maxSpaceLenLeqLineLen
                        = Nat.leqStep sMaxSpaceLen sLineLen sTotalHintsLen
                            Proof.Refl

                      spaceLenLeqMaxSpaceLen
                         = lneqSuccToLeq sSpaceLen sMaxSpaceLen Proof.Witness

                      spaceLenLeqLineLen
                        = Nat.leqTrans sSpaceLen sMaxSpaceLen sLineLen
                            spaceLenLeqMaxSpaceLen maxSpaceLenLeqLineLen

                      sRestHintsLen = SNat @ restHintsLen

                      sCommittedLen = sTotalHintsLen %:+ sSpaceLen

                      sRemainingSpaceLen = sLineLen %:- sCommittedLen

                      blockLenLeqTotalHintsLen
                        = Nat.leqStep (hint ^. run) sTotalHintsLen sRestHintsLen
                            Proof.Refl

                      sLineMinusSpaceLen = sLineLen %:- sSpaceLen

                      {-
                      totalHintsLen + maxSpaceLen = lineLen
                      spaceLen + sLineMinusSpaceLen = lineLen
                      spaceLen <= maxSpaceLen
                      totalHintsLen + spaceLen + remainingSpaceLen = lineLen
                      totalHintsLen + remainingSpaceLen = lineLen - spaceLen
                      ∵ totalHintsLen <= lineLen - spaceLen
                      -}

                      totalHintsLenLeqLineMinusSpaceLen
                        = Nat.leqStep sTotalHintsLen sLineMinusSpaceLen sRemainingSpaceLen Proof.Refl

                      blockLenLeqLineMinusSpaceLen
                        = Nat.leqTrans (hint ^. run) sTotalHintsLen sLineMinusSpaceLen
                            blockLenLeqTotalHintsLen totalHintsLenLeqLineMinusSpaceLen

                  withMatchingHint spaceLenLeqLineLen (Hint Empty sSpaceLen) line
                      ( \afterSpace
                         -> withMatchingHint blockLenLeqLineMinusSpaceLen hint afterSpace
                              ( \rest
                                 -> _
                              )
                      )

{-
    do  let sMaxSpaceLen = sLineLen %:- sTotalHintsLen
        Ord.OLt sSpaceLen <- Ord.enumOrdinal (sMaxSpaceLen %:+ (SNat @ 1))
        let spaceLenLeqMaxSpaceLen = lneqSuccToLeq sSpaceLen sMaxSpaceLen Proof.Witness
            maxSpaceLenLeqLineLen = Nat.plusLeqL sMaxSpaceLen sTotalHintsLen
            z = Nat.leqTrans sSpaceLen sMaxSpaceLen sLineLen spaceLenLeqMaxSpaceLen maxSpaceLenLeqLineLen
        withWitness z $ withMatchingHint (Hint Empty sSpaceLen) line
          ( \postSpace
              ->  withMatchingHint hint postSpace
                    ( \rest -> (   Sized.append (Sized.replicate sSpaceLen Empty)
                               .   Sized.append (Sized.replicate (hint ^. run) (hint ^. value))
                               <$> infer sHintsLen sRestLen hints rest
                               )
                    )
          )
-}


leqToLneqSucc :: SNat n -> SNat m -> Proof.IsTrue (n :<= m)
                   -> Proof.IsTrue (n :< m + 1)
leqToLneqSucc n m Proof.Witness
  = case Sing.sCompare n m
      of Sing.SLT -> Nat.lneqSuccStepR n m Proof.Witness
         Sing.SEQ -> Proof.withRefl (Nat.eqToRefl n m Proof.Refl)
                      $ Nat.lneqSucc m


lneqSuccToLeq :: SNat n -> SNat m -> Proof.IsTrue (n :< m + 1)
                  -> Proof.IsTrue (n :<= m)
lneqSuccToLeq n m Proof.Witness
  = case Nat.zeroOrSucc n of
        Nat.IsZero -> Nat.leqZero m
        Nat.IsSucc nPred -> ( Proof.withRefl (Nat.succLneqSucc nPred m)
                            . Proof.withRefl (Nat.lneqSuccLeq nPred m)
                            $ Proof.Witness
                            )


{-
                        of Nothing -> []
                           Just postSpace
                             -> case matchHint hint postSpace
                                  of Nothing -> []
                                     Just rest
                                       -> ( Sized.append (Sized.replicate sSpaceLen Empty)
                                          . Sized.append (Sized.replicate (hint ^. run) (hint ^. value))
                                          )
                                            <$> infer sHintsLen sRestLen hints' rest
        )
-}


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


withMatchingHint :: forall hintLen lineLen f a b
                  . ( Alternative f
                    , Eq a
                    )
                 => Proof.IsTrue (hintLen :<= lineLen)
                    -> Hint hintLen (Cell a)
                    -> LineKnowledge lineLen a
                    -> (LineKnowledge (lineLen - hintLen) a -> f b)
                    -> f b
withMatchingHint Proof.Witness hint line matchCont
  = let (block, rest) = Sized.splitAt (hint ^. run) line
     in if any (can'tMatch (hint ^. value)) block
          then  empty
          else  matchCont rest


natVal :: SNat n -> Natural
natVal n@SNat = fromInteger $ Sing.natVal n


natSing :: Natural -> SomeSing Nat
natSing = toSing . fromIntegral


withSomeNat :: Natural -> (forall n. SNat n -> r) -> r
withSomeNat n r = withSomeSing (fromIntegral n) (\s -> withKnownNat s (r s))
