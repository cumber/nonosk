{-# LANGUAGE FlexibleInstances
          , MultiParamTypeClasses
  #-}

module Scaffolding.SmallCheck
  ()
where

import Test.SmallCheck.Series ( Serial (series)
                              , generate
                              )

import Data.Indexed.Index ( Index
                          , someIndex
                          )
import Data.Indexed.Some ( Some )


instance Monad m => Serial m (Some Index ())
  where series = generate $ \d -> someIndex <$> [0 .. fromIntegral d]
