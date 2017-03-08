module Data.Indexed.Util.ShowsList
  ( showsList )
where

import Data.List ( intersperse )


showsList :: String -> String -> String -> [ShowS] -> ShowS
showsList open close sep xs
  = ( showString open
    . foldr (.) (showString close) (intersperse (showString sep) xs)
    )
