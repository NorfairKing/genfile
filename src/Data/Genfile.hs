{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Genfile
    ( GenFile(..)
    , GenDirTree

    , genFile
    , inDir
    , rootDirTree
    , renderDirTree
    , renderDirTreeString
    ) where

import           Control.Monad.Writer
import           Data.List            (nub)
import           Path
import           Path.IO              (ensureDir)

import           Data.Genfile.Types

-- | Generate a file in a writer monad.
genFile :: MonadWriter (GenDirTree rel str) m => Path rel File -> str -> m ()
genFile path contents = tell [GenFile path contents]

-- | Generate all the files in the given writer action under the given
-- directory.
inDir :: MonadWriter (GenDirTree Rel str) m => Path Rel Dir -> m a -> m a
inDir dir = censor $ map (\(GenFile p c) -> GenFile (dir </> p) c)

-- | Root a tree of files relative to a relative directory.
--
-- Use this function to make a 'GenDirTree' absolute so that you can render it
-- with 'renderDirTree'.
rootDirTree :: Path rel Dir -> GenDirTree Rel str -> GenDirTree rel str
rootDirTree dir = map (\(GenFile p c) -> GenFile (dir </> p) c)

-- | Render a tree of files according to a given function that decides on an
-- action to perform for every file.
--
-- This function first ensures that all required directories exist.
renderDirTree
    :: MonadIO m
    => GenDirTree Abs str
        -- ^ A tree of files
    -> (Path Abs File -> str -> m ())
        -- ^ The function thaat determines an action for every file.
    -> m ()
renderDirTree dt func = do
    mapM_ ensureDir $ nub $ map (\(GenFile p _) -> parent p) dt
    forM_ dt $ \(GenFile path contents) -> func path contents

-- | Render a tree of files containing 'String's by writing each to their file.
renderDirTreeString :: MonadIO m => GenDirTree Abs String -> m ()
renderDirTreeString dt = renderDirTree dt $ \p c ->
    liftIO $ writeFile (toFilePath p) c
