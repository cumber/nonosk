{-# LANGUAGE TemplateHaskell #-}

{-|
Provides literal syntax for 'Vector's via quasiquotation.

Use a @LANGUAGE QuasiQuotes@ pragma or @-XQuasiQuotes@; see
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html
for more information.
-}
module Data.Indexed.Vector.QQ
  ( vector
  )
where

import Language.Haskell.TH.Quote ( QuasiQuoter )


import Data.Indexed.Vector ( Vector ( (:^)
                                    , Nil
                                    )
                           )

import Data.Indexed.Util.QuasiList ( quasiList
                                   , fromConstructors
                                   )

{-|
>>> [vector| 1, 2, 3 |]  ==  1 :^ 2 :^ 3 ^: Nil
True

The type is properly inferred from the number of elements:

>>> :t [vector| True, False |]
Vector 2 Bool

Arbitrary Haskell expressions are allowed for the elements:

>>> [vector| Just $ 2 * 3, (\x -> if even x then Just (x + 1) else Nothing) 3 |]
Just 6 :^ Nothing :^ Nil
-}
vector :: QuasiQuoter
vector = quasiList $ fromConstructors '(:^) 'Nil
