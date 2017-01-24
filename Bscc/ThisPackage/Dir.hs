-- Copyright © 2012 Iain Nicol

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE CPP #-}

-- | In a fairly general way, this module provides details about this
-- specific package.  It is a bit analogous to config.h in Autotools
-- projects.  Additionally, it provides the means to determine the paths
-- of our data resources.
module Bscc.ThisPackage.Dir (getDataFileName, prefix) where

import Prelude hiding (FilePath)
#ifdef BUILD
import System.Environment (getExecutablePath)
#endif
import System.Path (AbsFile, RelFile)
#ifdef BUILD
import qualified System.Path as Path
#else
import System.Path (makeAbsoluteFromCwd)
#endif

prefix :: IO String
prefix = do
#ifdef BUILD
  exe <- getExecutablePath
  -- e.g. /usr/bin/foo -> /usr
  let parent = Path.toString . Path.takeDirectory . Path.absFile
  return (parent . parent $ exe)
#else
  return "/usr/local"
#endif

-- | The returned computation gives the path which should be used to
-- access data files from this package.
--
-- the function here can be used in an interpreter.
getDataFileName :: RelFile -- ^ Path to the data file.  This should be
                            -- relative to the top level directory of
                            -- this package.  The data file must be
                            -- listed under @data-files@ in our .cabal
                            -- file.
                   -> IO AbsFile
#ifdef BUILD
-- We can't use the Cabal provided 'Paths_bscc.getDataFileName'.  When
-- we build under stack, it will resolve to a subdirectory of the
-- .stack-work build directory.  That doesn't work because we want a
-- sensible directory for installation, not a build implementation
-- detail.
getDataFileName f = do
  prefix' <- prefix
  return $ Path.absFile $
    prefix' ++ "/share/brainsick/" ++ (Path.toString f)
#else
-- Similarly we can't use the Cabal provided function here, because it
-- won't exist when running inside a Haskell interpreter.
getDataFileName = makeAbsoluteFromCwd
#endif
