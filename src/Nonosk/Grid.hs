{-# LANGUAGE DeriveFunctor
           , TemplateHaskell
  #-}

module Nonosk.Grid
  ( Cell (..), Cell'
  , maybeCell
  , withCell

  , Knowledge (..)
  , maybeKnowledge
  , withKnowledge

  , Line, Line', LineKnowledge, LineKnowledge'
  , Grid, GridKnowledge

  , cellToAscii
  , asciiToCell
  , gridToAscii
  , gridToAsciiLines
  , asciiToGrid
  , asciiLinesToGrid
  )
where

import qualified Control.Lens as Lens

import Control.Monad ( (<=<) )

import Data.Semigroup ( Semigroup ((<>), stimes)
                      , stimesIdempotent
                      )

import Data.Indexed.Some ( Some2 )

import Data.Indexed.Vector ( Vector )

import Data.Indexed.Vector2 ( Vector2 )
import qualified Data.Indexed.Vector2 as Vector2


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

instance Eq a => Semigroup (Knowledge a)
  where kx <> ky
          | Known x <- kx
          , Known y <- ky
          , x == y
          = kx
          | otherwise
          = Unknown

        stimes = stimesIdempotent

-- | A Line is a 'Vector' of 'Cell's
type Line n a = Vector n (Cell a)

-- | A LineKnowledge is a 'Vector' of 'Knowledge' 'Cell's
type LineKnowledge n a = Vector n (Knowledge (Cell a))

-- | A Grid is a 'Vector2' of 'Cells's
type Grid r c a = Vector2 r c (Cell a)

-- | A GridKnowledge is a 'Vector2' of 'Knowledge 'Cell's
type GridKnowledge r c a = Vector2 r c (Knowledge (Cell a))

-- | A Line' is a 'Vector' of 'Cell''s
type Line' n = Vector n Cell'

-- | A @LineKnowledge' is a 'Vector' of @'Knowledge' 'Cell''@s
type LineKnowledge' n = Vector n (Knowledge Cell')


cellToAscii :: Knowledge (Cell ()) -> Char
cellToAscii (Known Empty) = ' '
cellToAscii (Known (Filled ())) = '\x2588'
cellToAscii Unknown = '\x2591'

asciiToCell :: Char -> Maybe (Knowledge (Cell ()))
asciiToCell ' ' = Just $ Known Empty
asciiToCell '#' = Just $ Known (Filled ())
asciiToCell '\x2588' = Just $ Known (Filled ())
asciiToCell '?' = Just Unknown
asciiToCell '\x2591' = Just Unknown
asciiToCell _ = Nothing

gridToAscii :: GridKnowledge r c () -> String
gridToAscii = unlines . gridToAsciiLines

gridToAsciiLines :: GridKnowledge r c () -> [String]
gridToAsciiLines = Vector2.toLists . fmap cellToAscii

asciiToGrid :: String -> Maybe (Some2 Vector2 (Knowledge (Cell ())))
asciiToGrid = asciiLinesToGrid . lines

asciiLinesToGrid :: [String] -> Maybe (Some2 Vector2 (Knowledge (Cell ())))
asciiLinesToGrid = Vector2.fromLists <=< sequenceA2 . (fmap . fmap) asciiToCell


sequenceA2 :: (Traversable s, Traversable t, Applicative f)
           => s (t (f a)) -> f (s (t a))
sequenceA2 = sequenceA . fmap sequenceA
