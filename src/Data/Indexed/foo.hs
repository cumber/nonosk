{-# LANGUAGE FlexibleContexts
           , ScopedTypeVariables
           , TypeApplications
           , TypeOperators
  #-}

module Foo where

import Data.Constraint ( (:-), (\\) )
import Data.Constraint.Forall (ForallF, instF)

data Nested q a = Stop | Deeper a (Nested q (q a))

data None a = None

instance Show (None a)
  where show None = "None"

instance (Show a, ForallF Show q) => Show (Nested q a)
  where show Stop = "Stop"
        show (Deeper a r) = show a ++ " " ++ show r
                              \\ (instF @Show @q @a)
