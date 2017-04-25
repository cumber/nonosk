{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE GADTs
           , DataKinds
           , DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , FlexibleInstances
           , MultiParamTypeClasses
           , PatternSynonyms
           , ScopedTypeVariables
           , StandaloneDeriving
           , TypeApplications
           , TypeFamilies
           , TypeOperators
  #-}

module Data.Indexed.Vector
  ( Vector (Nil, (:^), (:^-))

  -- * Basic functions
  , append
  , (++)

  , elemAt
  , (!)
  , head
  , last
  , tail
  , init
  , uncons

  , indexLength
  , length

  -- * Vector transformations
  , transpose
  , mapWithIndices
  , modifyElem

  -- * Building vectors
  , fromList
  , fromListIndexed
  , fromListIndexed'
  , replicate
  , replicate'
  , enumerate
  , enumerate'
  , fromIndices
  , fromIndices'

  -- * Extracting sub-vectors
  , take, take'
  , drop, drop'
  , splitAt, splitAt'

  -- * Zipping and unzipping vectors
  , zipWith
  , unzip
  )
where

import Prelude hiding ( head, last
                      , tail, init
                      , take, drop
                      , length
                      , replicate
                      , splitAt
                      , (++)
                      , unzip
                      , zipWith
                      )

import Data.Constraint ( Dict (Dict)
                       , (:-) (Sub)
                       , (\\)
                       )
import Data.Constraint.Nat ( leTrans )

import Data.Foldable ( toList )

import Data.Monoid ( Monoid (..) )
import Data.Semigroup ( Semigroup ((<>)) )

import qualified GHC.Exts as Exts


import Data.Indexed.Fin ( Fin
                        , fromFin
                        )
import qualified Data.Indexed.Fin as Fin

import Data.Indexed.ForAnyKnownIndex ( ForAnyKnownIndex (instAnyKnownIndex)
                                     , ForAnyKnownIndexF (instAnyKnownIndexF)
                                     )

import Data.Indexed.Index ( Index (Index)
                          , index'
                          , indexOf
                          , switchZero'
                          )
import Data.Indexed.Some ( Some (Some)
                         , liftPlus
                         )

import Data.Indexed.Nat ( KnownNat
                        , type (+), type (-), type (×)
                        , type (<=), type (>=)
                        )

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
pattern (:^-) :: () => (n >= 1) => a -> Vector (n - 1) a -> Vector n a
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


instance Eq a => ForAnyKnownIndex Eq Vector a
  where instAnyKnownIndex = Sub Dict

instance ForAnyKnownIndexF Functor Vector
  where instAnyKnownIndexF = Sub Dict

instance ForAnyKnownIndexF Foldable Vector
  where instAnyKnownIndexF = Sub Dict

instance ForAnyKnownIndexF Traversable Vector
  where instAnyKnownIndexF = Sub Dict

instance Show a => ForAnyKnownIndex Show Vector a
  where instAnyKnownIndex = Sub Dict


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
the second result of @f@ applied to the second element of @xs@, and so on. It's
lazy in the values of all the off-diagonal elements, but does have to traverse
the lists to skip over the leading ones, so this isn't particularly efficient.
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


fromListIndexed :: Index n () -> [a] -> Maybe (Vector n a)
fromListIndexed Index = fromListIndexed'

fromListIndexed' :: forall n a. KnownNat n => [a] -> Maybe (Vector n a)
fromListIndexed'
  = switchZero' @ n  mustBeEmpty oneMore
  where mustBeEmpty []      = Just Nil
        mustBeEmpty (_ : _) = Nothing

        oneMore :: forall m b. (KnownNat m, m >= 1) => [b] -> Maybe (Vector m b)
        oneMore [] = Nothing
        oneMore (x : xs) = (x :^) <$> fromListIndexed' @ (m - 1) xs


instance Exts.IsList (Some Vector a)
  where type Item (Some Vector a) = a
        fromList = fromList
        toList = toList


fromIndices' :: (KnownNat n, Integral i) => (Fin n i -> a) -> Vector n a
fromIndices' gen = gen <$> enumerate'

fromIndices :: Integral i => Index n () -> (Fin n i -> a) -> Vector n a
fromIndices Index = fromIndices'


mapWithIndices :: (KnownNat n, Integral i)
               => (Fin n i -> a -> b) -> (Vector n a -> Vector n b)
mapWithIndices = flip zipWith enumerate'



indexLength :: KnownNat n => Vector n a -> Index n ()
indexLength = indexOf

{-|
Note that the 'length' from the 'Foldable' instance for 'Vector' (and thus also
for 'Some' 'Vector') obtains the length by direct measurement (traversal). This
function instead just converts the type index directly to value, which is a
constant time instead of linear cost.

It does require a 'KnownNat' constraint on the length, howver, which is why the
'length' in the 'Foldable' instance does not use this strategy; it would require
the 'Foldable' instance to have a 'KnownNat n' constraint, which would infect
many other operations that otherwise do not need it.
-}
length :: forall a n. (KnownNat n, Integral a) => Vector n a -> a
length _ = fromIntegral $ index' @ n


append :: Vector n a -> Vector m a -> Vector (n + m) a
append Nil ys = ys
append (x :^ xs) ys = x :^ append xs ys

(++):: Vector n a -> Vector m a -> Vector (n + m) a
(++) = append


zipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWith _ Nil _ = Nil
zipWith f (x :^ xs) (y :^ ys) = f x y :^ zipWith f xs ys


unzip :: Vector n (a, b) -> (Vector n a, Vector n b)
unzip Nil = (Nil, Nil)
unzip ((x, y) :^ xys)
  = case unzip xys
      of ~(xs, ys) -> (x :^ xs, y :^ ys)


crossWith :: (a -> b -> c) -> Vector n a -> Vector m b -> Vector (n × m) c
crossWith _ Nil _ = Nil
crossWith f (x :^ xs) ys = (f x <$> ys) ++ crossWith f xs ys


replicate' :: forall n a. KnownNat n => a -> Vector n a
replicate' x
  = switchZero' @ n
      Nil
      (x :^ replicate' @ (n - 1) x)


replicate :: Index n () -> a -> Vector n a
replicate Index = replicate'


enumerate' :: forall n a. (KnownNat n, Integral a) => Vector n (Fin n a)
enumerate'
  = switchZero' @ n
      Nil
      (go @ n minBound)
  where go :: forall m. KnownNat m => Fin n a -> Vector m (Fin n a)
        go x = switchZero' @ m
                 Nil
                 (x :^ go @ (m - 1) (succ x))

enumerate :: Integral a => Index n () -> Vector n (Fin n a)
enumerate Index = enumerate'


elemAt :: (n >= 1, Integral i) => Fin n i -> Vector n a -> a
elemAt = go . fromFin
  where go :: Integral i => i -> Vector n a -> a
        go i (x :^ xs)
          | i == 0     = x
          | otherwise  = go (pred i) xs

(!) :: (n >= 1, Integral i) => Vector n a -> Fin n i -> a
(!) = flip elemAt

head :: (n >= 1) => Vector n a -> a
head (x :^ _) = x

last :: (n >= 1) => Vector n a -> a
last (x :^ xs) = go x xs
  where go :: a -> Vector n a -> a
        go y Nil = y
        go _ (y :^ ys) = go y ys

tail :: (n >= 1) => Vector n a -> Vector (n - 1) a
tail (_ :^ xs) = xs

init :: (n >= 1) => Vector n a -> Vector (n - 1) a
init (x :^ xs) = go x xs
  where go :: a -> Vector n a -> Vector n a
        go _ Nil = Nil
        go y (y' :^ ys) = y :^ go y' ys

uncons :: (n >= 1) => Vector n a -> (a, Vector (n - 1) a)
uncons xs = (head xs, tail xs)


transpose :: KnownNat m => Vector n (Vector m a) -> Vector m (Vector n a)
transpose = sequenceA
{- TODO: turn this into a quickcheck?
transpose Nil = replicate' Nil   -- 0 rows of n cols -> n rows of 0 cols
transpose (row :^ rows) = zipWith (:^) row $ transpose rows
-}

modifyElem :: Integral i => Fin n i -> (a -> a) -> Vector n a -> Vector n a
modifyElem _ _ Nil = Nil
modifyElem i f (x :^ xs)
  = maybe (f x :^ xs) (\i' -> x :^ modifyElem i' f xs) $ Fin.down i


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
  = switchZero' @ k
      (Nil, xs)
      ( case splitAt' @ (k - 1) (tail xs)
          of (hs, ts) -> (head xs :^ hs, ts)
         \\ minusMonotone1 @ k @ n @ 1
         \\ leTrans @ 1 @ k @ n
      )

splitAt :: forall s n a. (s <= n)
        => Index s () -> Vector n a -> (Vector s a, Vector (n - s) a)
splitAt Index = splitAt'
