{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Genfile
  ( GenFile(..)
  , GenDirTree
  , renderDirTree
  , genFile
  , inDir
  , rootDirTree
  ) where

import Control.Monad.Writer
import Path

import Data.Genfile.Render
import Data.Genfile.Types

-- | Generate a file in a writer monad.
genFile
  :: MonadWriter (GenDirTree rel str) m
  => Path rel File -> str -> m ()
genFile path contents = tell [GenFile path contents]

-- | Generate all the files in the given writer action under the given
-- directory.
inDir
  :: MonadWriter (GenDirTree Rel str) m
  => Path Rel Dir -> m a -> m a
inDir dir = censor $ map (\(GenFile p c) -> GenFile (dir </> p) c)

-- | Root a tree of files relative to a relative directory.
--
-- Use this function to make a 'GenDirTree' absolute so that you can render it
-- with 'renderDirTree'.
rootDirTree :: Path rel Dir -> GenDirTree Rel str -> GenDirTree rel str
rootDirTree dir = map (\(GenFile p c) -> GenFile (dir </> p) c)
