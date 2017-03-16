{-# LANGUAGE GADTs
           , DataKinds
           , DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , FlexibleInstances
           , PatternSynonyms
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeApplications
           , TypeFamilies
           , TypeOperators
  #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Data.Indexed.Vector
  ( Vector (Nil, (:^), (:^-))

  -- * Basic functions
  , append
  , (++)

  , head
  , last
  , tail
  , init
  , uncons

  , indexLength

  -- * Vector transformations
  , transpose

  -- * Building vectors
  , fromList
  , replicate
  , replicate'

  -- * Extracting sub-vectors
  , take, take'
  , drop, drop'
  , splitAt, splitAt'

  , inc, double
  )
where

import Prelude ( Functor (..), (<$>)
               , Applicative (..)
               , Monad (..)
               , Show (..), showsPrec
               , Eq (..)
               , Ord (..)
               , Traversable (..)
               , ($), (.), id
               , fst, snd
               , Int, (+), (*)
               )

import Data.Constraint ( (\\) )
import Data.Constraint.Nat ( leTrans )

import Data.Foldable ( Foldable (..) )

import Data.Monoid ( Monoid (..) )
import Data.Semigroup ( Semigroup ((<>)) )

import qualified GHC.Exts as Exts

import GHC.TypeLits ( type (+)
                    , type (-)
                    , type (<=)
                    , KnownNat
                    )
import qualified GHC.TypeLits as TL


import Data.Indexed.Index
import Data.Indexed.Some

import Data.Indexed.Util.ShowsListLike ( showsListLike )
import Data.Indexed.Util.NatProofs ( minusMonotone1 )


data Vector n a
  where Nil  :: Vector 0 a
        (:^) :: a -> Vector n a -> Vector (n + 1) a
infixr 5 :^


{-|
Pattern matching on a @Vector n a@ using the @':^'@ constructor gives you a new
@Vector n0 a@ for the tail, where @n ~ (n0 + 1)@.

Sometimes its more convenient to instead work with the tail as @Vector n1 a@,
where @n1 ~ (n - 1)@. The two are of course equivalent but the compiler doesn't
always realise this without an explicit demonstration.
-}
pattern (:^-) :: () => (1 <= n) => a -> Vector (n - 1) a -> Vector n a
pattern x :^- xs = x :^ xs
infixr 5 :^-


deriving instance Eq a => Eq (Vector n a)
deriving instance Ord a => Ord (Vector n a)
deriving instance Functor (Vector n)
deriving instance Foldable (Vector n)
deriving instance Traversable (Vector n)


instance Show a => Show (Vector n a)
  where showsPrec p
          = showsListLike p ":^" 5 "Nil" . fmap (showsPrec 5) . toList


{-|
For @Vector n@ to be Applicative, '<*>' has to take and return vectors of the
same statically known length; so Vector's Applicative behaviour is similar to
'Control.Applicative.ZipList' rather than normal lists; it zips rather than
performing a cartesian join.

Since the lengths are statically known and always match, 'pure' returns a Vector
of the type-appropriate length, in contrast to ZipList's pure which must return
an infinite list so that the truncating behaviour of zipping mismatched lists
never cuts off another list's tail.
-}
instance KnownNat n => Applicative (Vector n)
  where pure = replicate'
        (<*>) = zipWith ($)


{-|
For @Vector n@ to be Monad, a nested monad is @Vector n (Vector n a)@; i.e. a
square matrix. @'Control.Monad.join'@ takes the diagonal.

@xs '>>=' f@ takes the first result of @f@ applied to the first element of @xs@,
the second element of @f@ applied to the second element of @xs@, and so on. It's
lazy in the values of all the off-diagonal elements, but does have to traverse
the lists to skip over them, so this isn't particularly efficient.
-}
instance KnownNat n => Monad (Vector n)
  where Nil >>= _ = Nil
        (x :^- xs) >>= f
          = (head . f $ x) :^ (xs >>= tail . f)


instance Semigroup (Some Vector a)
  where (<>) = liftPlus append

instance Monoid (Some Vector a)
  where mempty = Some Nil
        mappend = (<>)


{-|
The Applicative instance for @Some Vector@ is not justthe @Vector n@ instance
with the lengths hidden; since the lengths are hidden they also can't be
required to match. So instead the cartesian join (or nondeterminism) behaviour
of the Applicative instance for @[]@ is the natural instance for @Some Vector@.
-}
instance Applicative (Some Vector)
  where pure = Some . (:^ Nil)
        Some fs <*> Some ys = Some $ crossWith id fs ys

{-|
See comments on the Applicative instance for @Some Vector@.
-}
instance Monad (Some Vector)
  where Some Nil >>= _ = Some Nil
        Some (x :^- xs) >>= f = f x <> (Some xs >>= f)


fromList :: [a] -> Some Vector a
fromList [] = Some Nil
fromList (x:xs) = case fromList xs
                    of Some v -> Some (x :^ v)


instance Exts.IsList (Some Vector a)
  where type Item (Some Vector a) = a
        fromList = fromList
        toList = toList


indexLength :: KnownNat n => Vector n a -> Index n ()
indexLength _ = Index


append :: Vector n a -> Vector m a -> Vector (n + m) a
append Nil ys = ys
append (x :^ xs) ys = x :^ append xs ys

(++):: Vector n a -> Vector m a -> Vector (n + m) a
(++) = append


zipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWith _ Nil _ = Nil
zipWith f (x :^ xs) (y :^ ys) = f x y :^ zipWith f xs ys


crossWith :: (a -> b -> c) -> Vector n a -> Vector m b -> Vector (n TL.* m) c
crossWith _ Nil _ = Nil
crossWith f (x :^ xs) ys = (f x <$> ys) ++ crossWith f xs ys


replicate' :: forall n a. KnownNat n => a -> Vector n a
replicate' x
  = switchZero (Index @ n)
      Nil
      (x :^ replicate' @ (n - 1) x)


replicate :: Index n () -> a -> Vector n a
replicate Index = replicate'


head :: (1 <= n) => Vector n a -> a
head (x :^ _) = x

last :: (1 <= n) => Vector n a -> a
last (x :^ xs) = go x xs
  where go :: a -> Vector n a -> a
        go y Nil = y
        go _ (y :^ ys) = go y ys

tail :: (1 <= n) => Vector n a -> Vector (n - 1) a
tail (_ :^ xs) = xs

init :: (1 <= n) => Vector n a -> Vector (n - 1) a
init (x :^ xs) = go x xs
  where go :: a -> Vector n a -> Vector n a
        go _ Nil = Nil
        go y (y' :^ ys) = y :^ go y' ys

uncons :: (1 <= n) => Vector n a -> (a, Vector (n - 1) a)
uncons xs = (head xs, tail xs)


transpose :: KnownNat m => Vector n (Vector m a) -> Vector m (Vector n a)
transpose Nil = replicate' Nil   -- 0 rows of n cols -> n rows of 0 cols
transpose (row :^ rows) = zipWith (:^) row $ transpose rows


take :: (KnownNat k, k <= n) => Index k () -> Vector n a -> Vector k a
take Index = take'

take' :: forall k n a. (KnownNat k, k <= n) => Vector n a -> Vector k a
take' = fst . splitAt'

drop :: (KnownNat k, k <= n) => Index k () -> Vector n a -> Vector (n - k) a
drop Index = drop'

drop' :: forall k n a. (KnownNat k, k <= n) => Vector n a -> Vector (n - k) a
drop' = snd . splitAt'

splitAt' :: forall k n a. (KnownNat k, k <= n)
         => Vector n a -> (Vector k a, Vector (n - k) a)
splitAt' xs
  = switchZero (Index @ k)
      (Nil, xs)
      ( case splitAt' @ (k - 1) (tail xs)
          of (hs, ts) -> (head xs :^ hs, ts)
         \\ minusMonotone1 @ k @ n @ 1
         \\ leTrans @ 1 @ k @ n
      )

splitAt :: forall s n a. (s <= n)
        => Index s () -> Vector n a -> (Vector s a, Vector (n - s) a)
splitAt Index = splitAt'


inc :: forall n. KnownNat n => Int -> Vector n Int
inc x = switchZero (Index @ n) Nil (x :^ inc @ (n - 1) (x + 1))

double :: forall n. KnownNat n => Int -> Vector n Int
double x = switchZero (Index @ n) Nil (x :^ double @ (n - 1) (x * 2))
