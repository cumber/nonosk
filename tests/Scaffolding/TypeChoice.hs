{-# LANGUAGE AllowAmbiguousTypes
           , ConstraintKinds
           , DataKinds
           , FlexibleInstances
           , GADTs
           , MultiParamTypeClasses
           , ScopedTypeVariables
           , TypeApplications
           , TypeFamilyDependencies
           , TypeOperators
           , UndecidableInstances
  #-}

module Scaffolding.TypeChoice
  ( TypeChoice (..)
  , choice
  , Choosable
  , choices
  )
where

import Data.Constraint ( Constraint )

import Data.Kind ( Type )

import Data.Proxy ( Proxy (Proxy) )

import Data.Typeable ( Typeable
                     , typeRep
                     )


type family t ∈ ts = (r :: Constraint)
  where t ∈ (t ': _) = ()
        t ∈ (_ ': ts) = t ∈ ts

-- Existential wrapper for a Proxy, which allows us to know after pattern
-- matching that the proxied type implements a constraint and is one of a
-- static list of choices
data TypeChoice (c :: Type -> Constraint) (choices :: [Type])
  where Choice :: (c t, Typeable t, t ∈ choices)
               => Proxy t -> TypeChoice c choices


choice :: forall t c ts. (c t, Typeable t, t ∈ ts) => TypeChoice c ts
choice = Choice $ Proxy @ t

choices :: forall c choices. Choosable c choices => [TypeChoice c choices]
choices = choicesInUniverse (Proxy @ choices)

type Choosable c ts = ChoicesInUniverse c ts ts

class ChoicesInUniverse c (univ :: [Type]) (ts :: [Type])
  where choicesInUniverse :: Proxy ts -> [TypeChoice c univ]

instance ChoicesInUniverse c u '[]
  where choicesInUniverse = const []

instance    (c t, Typeable t, t ∈ u, ChoicesInUniverse c u ts)
         => ChoicesInUniverse c u (t ': ts)
  where choicesInUniverse _ = Choice (Proxy @ t) : choicesInUniverse (Proxy @ ts)


instance Show (TypeChoice c ts)
  where show (Choice p)
          = "@" ++ show (typeRep p)
