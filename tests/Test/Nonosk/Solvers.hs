{-# LANGUAGE DeriveGeneric
           , FlexibleInstances
           , MultiParamTypeClasses
  #-}

module Test.Nonosk.Solvers
  ( tests )
where

import Control.Applicative ( Alternative (empty) )

import Control.Monad ( replicateM )

import Data.Maybe ( catMaybes )

import Data.List.HT ( sliceVertical )

import GHC.Generics ( Generic )

import Test.SmallCheck.Series ( Serial (series)
                              , Series
                              , Positive (Positive)
                              , generate
                              , decDepth
                              )
import Test.Tasty ( TestTree
                  , testGroup
                  )
import qualified Test.Tasty.SmallCheck as SC


import Data.Indexed.Some ( Some
                         , Some2
                         )

import Data.Indexed.Vector ( Vector )
import qualified Data.Indexed.Vector as Vector

import Data.Indexed.Vector2 ( Vector2 )
import qualified Data.Indexed.Vector2 as Vector2

import Nonosk.Grid
import Nonosk.Hints
import qualified Nonosk.ListSolver as ListSolver
import qualified Nonosk.PathSolver as PathSolver

import Scaffolding.SmallCheck


tests :: TestTree
tests = testGroup "Solvers" []



data KnownCell a
  = KnownCell { cell :: Cell a
              , known :: Bool
              }
  deriving (Eq, Show, Generic)

{-
TODO: finish this
knownLineToHints :: Vector n (KnownCell a) -> Hints n a
knownLineToHints
  = foldr _ _
-}
