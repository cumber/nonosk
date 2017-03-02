{-# LANGUAGE GADTs
           , FlexibleContexts
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeApplications
           , TypeInType
           , TypeFamilies
           , TypeOperators
  #-}

module Data.Indexed.Vector.Capped
  ( Capped (..) )
where

import Data.Constraint
import Data.Constraint.Lifting

import Data.Kind (Type)

import GHC.TypeLits ( type (<=)
                    , KnownNat
                    , Nat
                    , natVal
                    )


import Data.Indexed.Vector (Vector)


{-|
An existential wrapper for Vector
-}
data Capped n f a
  where Capped :: (KnownNat l, l <= n) => f l a -> Capped n f a

instance (LiftingForAll Show f, Show a) => Show (Capped n f a)
  where show (Capped (x :: f l a))
          = "Capped " ++ show x
              \\ liftingForAll @ Show @ f @ a @ l


class LiftingForAll p f
  where liftingForAll :: p a :- p (f t a)

instance LiftingForAll Show Vector
  where liftingForAll = Sub Dict
