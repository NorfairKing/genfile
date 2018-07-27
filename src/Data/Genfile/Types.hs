module Data.Genfile.Types
    ( GenFile(..)
    , GenDirTree
    ) where

import           Path

-- | The instructions to generate a file with relativity 'rel' and contents 'str'.
--
-- 'rel' is either 'Rel' or 'Abs', both from the 'path' package.
-- 'str' can be anything, but is usually something string-like.
data GenFile rel str = GenFile (Path rel File) str
    deriving (Show, Eq)

-- | A collection of 'GenFile's
type GenDirTree rel a = [GenFile rel a]
