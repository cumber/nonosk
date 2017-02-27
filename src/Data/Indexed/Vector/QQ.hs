{-# LANGUAGE TemplateHaskell #-}

{-|
Provides literal syntax for 'Vector's via quasiquotation.

Use a @LANGUAGE QuasiQuotes@ pragma or @-XQuasiQuotes@; see https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html for more information.
-}
module Data.Indexed.Vector.QQ
  ( vector
  , v
  )
where

import Language.Haskell.Meta (parseExp)

import Language.Haskell.TH
  ( Q
  , Exp ( ConE
        , InfixE
        , ListE
        )
  )
import Language.Haskell.TH.Quote
  ( QuasiQuoter ( QuasiQuoter
                , quoteExp
                , quotePat
                , quoteType
                , quoteDec
                )
  )


import Data.Indexed.Vector

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
vector = QuasiQuoter { quoteExp = vectorExp
                     , quotePat = error "vector cannot be used in patterns"
                     , quoteType = error "vector cannot be used in types"
                     , quoteDec = error "vector cannot be used as a \
                                        \top level declaration"
                     }

{-|
Just a shorter name for vector:

>>> [v| 1, 2, 3, 4, 5 |]
1 :^ 2 :^ 3 :^ 4 :^ 4 :^ 5 :^ Nothing
-}
v :: QuasiQuoter
v = vector


vectorExp :: String -> Q Exp
vectorExp src
  = eitherToQ $ listExpToVector =<< parseExp ("[" ++ src ++ "]")


listExpToVector :: Exp -> Either String Exp

listExpToVector (ListE es)
  = Right $ foldr (\x xs -> InfixE (Just x) (ConE '(:^)) (Just xs))
                  (ConE 'Nil)
                  es

listExpToVector _
  = Left "internal error: shouldn't be attempting to convert non-list \
         \Exp to Vector"


eitherToQ :: Either String a -> Q a
eitherToQ = either fail pure
