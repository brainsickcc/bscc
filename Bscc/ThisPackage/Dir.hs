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
module Bscc.ThisPackage.Dir (getDataFileName) where

#ifdef BUILD
import Control.Applicative ((<$>))
#endif
import Prelude hiding (FilePath)
import System.Path (AbsFile, RelFile)
#ifdef BUILD
import System.Path (asAbsFile, getPathString)
-- When our code is properly built, Cabal provides functions to
-- determine the paths of our resources.  When running inside GHCI it
-- does not.
import qualified Paths_bscc
#else
import System.Path (makeAbsoluteFromCwd)
#endif

-- | The returned computation gives the path which should be used to
-- access data files from this package.  Use this in preference to the
-- `Paths_bscc.getDataFileName' function from the Cabal generated
-- "Paths_bscc" module; the function here can additionally be used in an
-- interpreter.
getDataFileName :: RelFile -- ^ Path to the data file.  This should be
                            -- relative to the top level directory of
                            -- this package.  The data file must be
                            -- listed under @data-files@ in our .cabal
                            -- file.
                   -> IO AbsFile
#ifdef BUILD
getDataFileName f = asAbsFile <$> Paths_bscc.getDataFileName (getPathString f)
#else
getDataFileName = makeAbsoluteFromCwd
#endif
