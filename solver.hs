module Main
  ( main )
where

import qualified Data.ByteString.Char8 as Char8

import System.Environment


import Data.Indexed.Some

import Nonosk.Grid
import Nonosk.FileFormats
import Nonosk.ListSolver


main :: IO ()
main
  = do  (filename : _) <- getArgs
        result <- parseSimplePuzzle <$> Char8.readFile filename
        case result
          of Left err
                -> putStrLn err
             Right puzzle
               -> mapM_ putStrLn $ forSome2 (fmap gridToAscii . iterateSolver) puzzle
