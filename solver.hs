module Main
  ( main )
where

import qualified Data.ByteString.Char8 as Char8

import System.Environment


import Data.Indexed.Some

import Nonosk.Puzzle
import Nonosk.FileFormats


main :: IO ()
main
  = do  (filename : _) <- getArgs
        Right puzzle <- parseSimplePuzzle <$> Char8.readFile filename
        mapM_ putStrLn $ forSome2 (fmap gridToAscii . solvePuzzleSteps) puzzle
