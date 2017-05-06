{-# LANGUAGE OverloadedStrings
           , TupleSections
  #-}

module Nonosk.FileFormats
  ( parseSimplePuzzle )
where

import Control.Arrow ( first )

import Control.Monad ( (<=<) )

import qualified Data.ByteString.Char8 as Char8

import Data.Char ( isDigit )

import Data.Semigroup ( (<>) )


import Data.Indexed.Some ( Some2 )
import Nonosk.Hints
import Nonosk.Grid
import Nonosk.Puzzle


nonoskNonogramHeader = "# nonosk nonogram"
nonoskNonogramHeaderLength = Char8.length nonoskNonogramHeader

parseSimplePuzzle :: Char8.ByteString -> Either String (Some2 Puzzle ())
parseSimplePuzzle = parseLines . Char8.lines


parseLines inputLines
  = do rest <- parseHeader inputLines
       (rows, rest') <- parseClues "rows" rest
       (cols, rest'') <- parseClues "columns" rest'
       if null rest''
         then maybe (Left "Invalid row and column clues") Right
                $ initPuzzle rows cols
         else do grid <- parseGrid rest''
                 maybe (Left "Invalid row and column clues or grid") Right
                    $ makePuzzle rows cols grid


parseHeader [] = Left "Empty input"
parseHeader (l : ls)
  = let (header, version) = Char8.splitAt nonoskNonogramHeaderLength l
     in if header == nonoskNonogramHeader
          then  if version == " 1"
                  then  Right ls
                  else  Left $ "Unknown file format version: "
                                 <> Char8.unpack version
          else  Left "Unrecognised file format; missing header"


parseClues label
  = sequenceFirst . first parseLineClues . span (startsWith isDigit)
      <=< parseSectionHeader label


parseLineClues = traverse (parseSingleLineClues . Char8.words)


parseSingleLineClues
  = maybe (Left "Invalid clue line") Right
      . traverse (fmap (`someHint` ()) . naturalWord)


sequenceFirst (fa, b) = (,b) <$> fa


naturalWord = nonNegative <=< restNull <=< Char8.readInteger


restNull (x, rest)
  | Char8.null rest  = Just x
  | otherwise        = Nothing


nonNegative i
  | i < 0      = Nothing
  | otherwise  = Just $ fromInteger i


parseSectionHeader label []
  = Left $ "Unexpected end of input; expecting section: "
             <> Char8.unpack label
parseSectionHeader label (l : ls)
  | l == "# " <> label  = Right ls
  | otherwise           = Left $ "Unexpected input; expecting section: "
                                   <> Char8.unpack label

startsWith p bs = (not . Char8.null) bs && (p . Char8.head) bs


parseGrid = parseGrid' <=< parseSectionHeader "grid"
  where parseGrid' = ( maybe (Left "Invalid grid") Right
                     . asciiLinesToGrid
                     . fmap Char8.unpack
                     )
