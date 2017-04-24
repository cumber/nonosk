module Main
  ( main )
where

import Test.Tasty ( TestTree
                  , testGroup
                  , defaultMainWithIngredients
                  , defaultIngredients
                  )

import Test.Tasty.Runners.Html ( htmlRunner )

import Test.Tasty.Ingredients.Rerun ( rerunningTests )


import qualified Test.Data.Indexed.Vector as Vector

main :: IO ()
main = defaultMainWithIngredients
         [ rerunningTests $ htmlRunner : defaultIngredients ]
         tests

tests :: TestTree
tests = testGroup "All" [ Vector.tests
                        ]
