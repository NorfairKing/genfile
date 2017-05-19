{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Genfile.String
    ( renderDirTreeString
    ) where

import Control.Monad.Writer
import Path

import Data.Genfile.Render
import Data.Genfile.Types

-- | Render a tree of files containing 'String's by writing each to their file.
renderDirTreeString
    :: MonadIO m
    => GenDirTree Abs String -> m ()
renderDirTreeString dt =
    renderDirTree dt $ \p c -> liftIO $ writeFile (toFilePath p) c
