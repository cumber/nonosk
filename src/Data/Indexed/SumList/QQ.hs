{-# LANGUAGE TemplateHaskell #-}

{-|
Provides literal syntax for 'SumList's via quasiquotation.

Use a @LANGUAGE QuasiQuotes@ pragma or @-XQuasiQuotes@; see https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html for more information.
-}
module Data.Indexed.SumList.QQ
  ( sumList
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


import Data.Indexed.SumList

{-|
>>> [sumList| 1, 2, 3 |]  ==  1 :^ 2 :^ 3 ^: Nil
True

The type is properly inferred from the number of elements:

>>> :t [sumList| True, False |]
SumList 2 Bool

Arbitrary Haskell expressions are allowed for the elements:

>>> [sumList| Just $ 2 * 3, (\x -> if even x then Just (x + 1) else Nothing) 3 |]
Just 6 :^ Nothing :^ Nil
-}
sumList :: QuasiQuoter
sumList = QuasiQuoter { quoteExp = sumListExp
                      , quotePat = error "sumList cannot be used in patterns"
                      , quoteType = error "sumList cannot be used in types"
                      , quoteDec = error "sumList cannot be used as a \
                                         \top level declaration"
                      }

sumListExp :: String -> Q Exp
sumListExp src
  = eitherToQ $ listExpToSumList =<< parseExp ("[" ++ src ++ "]")


listExpToSumList :: Exp -> Either String Exp

listExpToSumList (ListE es)
  = Right $ foldr (\x xs -> InfixE (Just x) (ConE '(:+)) (Just xs))
                  (ConE 'Nil)
                  es

listExpToSumList _
  = Left "internal error: shouldn't be attempting to convert non-list \
         \Exp to SumList"


eitherToQ :: Either String a -> Q a
eitherToQ = either fail pure
