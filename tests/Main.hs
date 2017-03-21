module Main
  ( main )
where

import Test.Tasty ( TestTree
                  , testGroup
                  , defaultMain
                  )


import qualified Test.Data.Indexed.Vector as Vector

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All" [ Vector.tests
                        ]
