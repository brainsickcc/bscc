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
module Bscc.Codegen.Machine (codegen,
                             withModuleFromAst,
                             withModuleFromLlAsmFile,
                             withModuleFromLlAsmString) where

import qualified Bscc.Triplet as Triplet

import Control.Exception (throwIO)
import Control.Monad ((>=>))
import Control.Monad.Trans.Error (ErrorT, runErrorT)
import qualified Data.ByteString as B
import qualified Data.Set as Set
import qualified LLVM.General.AST as A
import qualified LLVM.General.Module as M
import qualified LLVM.General.CodeGenOpt as CodeGenOpt
import qualified LLVM.General.CodeModel as CodeModel
import LLVM.General.Context (withContext)
import qualified LLVM.General.Target as T
import qualified LLVM.General.Relocation as Relocation
import Prelude hiding (FilePath, readFile)
import System.Path (AbsFile)
import qualified System.Path as Path
import System.Path.IO (readFile)

-- | Bracket the creation of a C++ LLVM Module.  Build the C++ LLVM
-- Module from a pure LLVM AST.
withModuleFromAst :: A.Module -> (M.Module -> IO r) -> IO r
withModuleFromAst ast action = withContext $ \context -> do
  fromRightErrorTIo $ M.withModuleFromAST context ast action

-- | Bracket the creation of a C++ LLVM Module by reading a file written
-- in LLVM assembly.
withModuleFromLlAsmFile :: AbsFile
                        -- ^ Path to the input file, which must be in
                        -- LLVM assembly.
                        -> (M.Module -> IO r)
                        -> IO r
withModuleFromLlAsmFile llFile action = do
  llText <- readFile llFile
  withModuleFromLlAsmString llText action

-- | Bracket the creation of a C++ LLVM Module from LLVM assembly.
withModuleFromLlAsmString :: String
                          -- ^ Path to the input file, which must be in
                          -- LLVM assembly.
                          -> (M.Module -> IO r)
                          -> IO r
withModuleFromLlAsmString llText action = do
  withContext $ \context -> do
    fromRightErrorTIo $ M.withModuleFromLLVMAssembly context llText action

-- | Generate an object file from an LLVM C++ Module.  The object file
-- is written to the given path.
codegen :: M.Module -> Triplet.Triplet -> AbsFile -> IO ()
codegen llCxxModule triple outPath = do
  target <- lookupTargetSafe triple
  T.withTargetOptions $ \targetOptions -> do
    T.withTargetMachine target (Triplet.str triple) (Triplet.cpu triple)
                        Set.empty
                        targetOptions Relocation.Default CodeModel.Default
                        CodeGenOpt.Default $
                        \machine -> do
                          bs <- fromRightErrorTIo
                                  (M.moduleObject machine llCxxModule)
                          B.writeFile (Path.toString outPath) bs

fromRightErrorTIo :: Show a => ErrorT a IO b -> IO b
fromRightErrorTIo = runErrorT >=> either (throwIO . userError . show) return

-- | This is safe in that it takes care of any required initialization.
lookupTargetSafe :: Triplet.Triplet -> IO T.Target
lookupTargetSafe triple = do
  T.initializeAllTargets
  (target, _) <- fromRightErrorTIo $
                 T.lookupTarget Nothing (Triplet.str triple)
  return target
