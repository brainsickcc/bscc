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

import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Control.Monad.Trans.Error (ErrorT, runErrorT)
import qualified Data.Set as Set
import qualified LLVM.General.Module as M
import qualified LLVM.General.CodeGenOpt as CodeGenOpt
import qualified LLVM.General.CodeModel as CodeModel
import LLVM.General.Context (withContext)
import qualified LLVM.General.Target as T
import qualified LLVM.General.Relocation as Relocation
import Prelude hiding (FilePath, readFile)
import System.Path (AbsFile, getPathString, replaceExtension)
import System.Path.IO (readFile)

-- | Code generation.
codegen :: Triplet.Triplet  -- ^ Target machine type.
           -> AbsFile       -- ^ Path to the input file, which must be
                            --   in the IR ("Bscc.Codegen.Ir").
           -> IO AbsFile    -- ^ The returned computation generates a
                            --   \".o\" object file, and returns its
                            --   path.
codegen triple llFile = do
  llText <- readFile llFile
  let outPath = llFile `replaceExtension` ".o"
  withContext $ \context -> do
    fromRightErrorTIo $
      M.withModuleFromString context llText $ \llCxxModule -> do
        codegenLlvmCxxModule llCxxModule triple outPath
  return outPath

-- | Generate an object file, writing it to the given path.
codegenLlvmCxxModule :: M.Module -> Triplet.Triplet -> AbsFile -> IO ()
codegenLlvmCxxModule llCxxModule triple outPath = do
  target <- lookupTargetSafe triple
  T.withTargetOptions $ \targetOptions -> do
    T.withTargetMachine target (Triplet.str triple) (Triplet.cpu triple)
                        Set.empty
                        targetOptions Relocation.Default CodeModel.Default
                        CodeGenOpt.Default $
                        \machine -> fromRightErrorTIo $
                                    M.writeObjectToFile machine
                                                        (getPathString outPath)
                                                        llCxxModule

fromRightErrorTIo :: Show a => ErrorT a IO b -> IO b
fromRightErrorTIo = runErrorT >=> either (throwIO . userError . show) return

-- | This is safe in that it takes care of any required initialization.
lookupTargetSafe :: Triplet.Triplet -> IO T.Target
lookupTargetSafe triple = do
  T.initializeAllTargets
  (target, _) <- fromRightErrorTIo $
                 T.lookupTarget Nothing (Triplet.str triple)
  return target
