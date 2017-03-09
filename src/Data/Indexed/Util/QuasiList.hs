{-|
Utility code for making "list-like" quasiquoters, for example:

@
[foo| e1, e2, e3 |]
@

where the e1, e2, etc elements are ordinary Haskell expressions, but the
constructed expression is using different types/constructors than standard
Haskell lists.

Use a @LANGUAGE QuasiQuotes@ pragma or @-XQuasiQuotes@; see https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html for more information.
-}
module Data.Indexed.Util.QuasiList
  ( quasiList
  , fromConstructors
  )
where

import Data.Semigroup ( (<>) )

import Language.Haskell.Exts ( ParseMode ( extensions )
                             , defaultParseMode
                             , parseExpWithMode
                             , Extension ( EnableExtension )
                             , KnownExtension ( DataKinds
                                              , GADTs
                                              , KindSignatures
                                              , TypeOperators
                                              , TypeApplications
                                              , QuasiQuotes
                                              )
                             )

import Language.Haskell.Meta ( parseResultToEither )
import Language.Haskell.Meta.Syntax.Translate ( toExp )

import Language.Haskell.TH
  ( Q
  , Name
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


cannotBeUsed = error . ("list-like quasiquoter cannot be used " <>)

{-|
Given a function that buils an expression from a list of sub-expressions,
makes a quasiquoter that parses a comma-separated list of Haskell expressions
and uses the supplied function to make an expressiona from them.
-}
quasiList :: ([Exp] -> Exp) -> QuasiQuoter
quasiList f
  = QuasiQuoter { quoteExp = listLikeExp f
                , quotePat = cannotBeUsed "in patterns"
                , quoteType = cannotBeUsed "in types"
                , quoteDec = cannotBeUsed "as a top level declaration"
                }


{-|
When using 'quasiList' to make a quasiquoter, if what you need to do to make a
single expression is apply constructors analagously to native lists' ':' and
'[]', you can just give the constructor names to this function and it will make
a @['Exp'] -> 'Exp'@ function suitable for using with quasiList.

For example @quasiList $ fromConstructors '(:) '[]@ would make a quasiquoter
that just duplicates native list syntax.
-}
fromConstructors :: Name -> Name -> ([Exp] -> Exp)
fromConstructors cons nil
  = foldr (\x xs -> InfixE (Just x) (ConE cons) (Just xs)) (ConE nil)


listLikeExp :: ([Exp] -> Exp) -> String -> Q Exp
listLikeExp f src
  = eitherToQ $ processListExp f
                  =<< toExp <$> parseResultToEither
                                  (parseListExp ("[" ++ src ++ "]"))


processListExp :: ([Exp] -> Exp) -> Exp -> Either String Exp

processListExp f (ListE es)
  = Right $ f es

processListExp _ _
  = Left "internal error: shouldn't be attempting to convert non-list \
         \Exp to Vector"


eitherToQ :: Either String a -> Q a
eitherToQ = either fail pure


parseListExp = parseExpWithMode indexedParseMode


indexedParseMode
  = defaultParseMode
      { extensions
          = map EnableExtension
              [ DataKinds
              , GADTs
              , KindSignatures
              , TypeOperators
              , TypeApplications
              , QuasiQuotes
              ]
      }
