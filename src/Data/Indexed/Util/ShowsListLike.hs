module Data.Indexed.Util.ShowsListLike
  ( showsListLike )
where

import Data.List ( intersperse )


showsListLike  :: Int -> String -> Int -> String -> [ShowS] -> ShowS
showsListLike prec cons consPrec nil items
  = ( showParen (prec >= consPrec)
    . foldr (.) id
    . intersperse showSpacedCons
    . (++ [showString nil])
    $ items
    )
  where showSpacedCons = showString $ (' ' : cons) ++ " "
