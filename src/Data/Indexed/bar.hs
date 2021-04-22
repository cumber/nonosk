{-# LANGUAGE FlexibleContexts
           , ScopedTypeVariables
           , TypeApplications
           , TypeOperators
  #-}

module Foo where

import Data.Constraint ( (:-), (\\) )
import Data.Constraint.Lifting ( Lifting (lifting) )

data Nested q a = Stop | Deeper a (Nested q (q a))

instance (Show a, Lifting Show q) => Show (Nested q a)
  where show Stop = "Stop"
        show (Deeper a r) = show a ++ " " ++ show r
                              \\ (lifting @Show @q @a)
