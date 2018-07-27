{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Genfile.Render
  ( renderDirTree
  ) where

import Control.Monad.Writer
import Data.List (nub)
import Path
import Path.IO (ensureDir)

import Data.Genfile.Types

-- | Render a tree of files according to a given function that decides on an
-- action to perform for every file.
--
-- This function first ensures that all required directories exist.
renderDirTree
  :: MonadIO m
  => GenDirTree Abs str
        -- ^ A tree of files
  -> (Path Abs File -> str -> m ())
        -- ^ The function that determines an action for every file.
  -> m ()
renderDirTree dt func = do
  mapM_ ensureDir $ nub $ map (\(GenFile p _) -> parent p) dt
  forM_ dt $ \(GenFile path contents) -> func path contents
