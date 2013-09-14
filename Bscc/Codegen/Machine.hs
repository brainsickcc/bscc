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

-- | Machine code generation.
module Bscc.Codegen.Machine (codegen) where

import qualified Bscc.Triplet as Triplet

import Control.Monad (void)
import Prelude hiding (FilePath)
import System.Path (AbsFile, getPathString, replaceExtension)
import System.Process (readProcess)

-- | Code generation.
codegen :: Triplet.Triplet  -- ^ Target machine type.
           -> AbsFile       -- ^ Path to the input file, which must be
                            --   in the IR ("Bscc.Codegen.Ir").
           -> IO AbsFile    -- ^ The returned computation generates a
                            --   \".o\" object file, and returns its
                            --   path.
codegen target llFile = do
  sFile <- toSFile target llFile
  toObjFile target sFile

-- | Conversion from IR to assembly language for the target.
toSFile :: Triplet.Triplet  -- ^ Target machine type.
           -> AbsFile       -- ^ Path to the input file, which must be
                            --   in the IR ("Bscc.Codegen.Ir").
           -> IO AbsFile    -- ^ The returned computation generates a
                            --   \".s\" assembly language file, and
                            --   returns its path.
toSFile target llFile = do
  let sFile = llFile `replaceExtension` ".s"
      cmd = "llc"
      args = ["-mtriple=" ++ Triplet.str target, "-o", getPathString sFile,
              getPathString llFile]
      stdin = []
  void $ readProcess cmd args stdin
  return sFile

-- | Assemble.
toObjFile :: Triplet.Triplet  -- ^ Target machine type
             -> AbsFile       -- ^ Path to file in assembly language of
                              --   the target machine.
             -> IO AbsFile    -- ^ The returned computation performs
                              --   assembly, generating a \".o\" object
                              --   file and returning its path.
toObjFile target sFile = do
  let objFile = sFile `replaceExtension` ".o"
      cmd = Triplet.str target ++ "-as"
      args = ["-o", getPathString objFile, getPathString sFile]
      stdin = []
  void $ readProcess cmd args stdin
  return objFile
