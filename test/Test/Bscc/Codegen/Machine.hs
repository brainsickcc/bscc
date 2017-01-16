-- Copyright © 2013 Iain Nicol

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

module Test.Bscc.Codegen.Machine (codegenMachineTests) where

import Bscc.Codegen.Ir (x86_stdcallcc)
import Bscc.Codegen.Machine
import Bscc.Triplet

import Data.List (isInfixOf)
import LLVM.General.AST as A
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Name as N
import qualified LLVM.General.AST.Type as AT
import qualified LLVM.General.AST.Visibility as V
import Prelude hiding (writeFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Path (relFile, (</>))
import qualified System.Path as Path
import System.Process (readProcess)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU

codegenMachineTests =
  T.testGroup "Bscc.Codegen.Machine"
  [
    HU.testCase "codegen" test_codegen,
    HU.testCase "withModuleFromAst doesn't throw"
                test_withModuleFromAstDoesntThrow,
    HU.testCase "withModuleFromLlAsmString doesn't throw"
                test_withModuleFromLlAsmStringDoesntThrow
  ]

simpleModule :: A.Module
simpleModule = defaultModule { moduleDefinitions = [GlobalDefinition f] }
  where f = G.Function L.External V.Default x86_stdcallcc []
                       AT.VoidType (N.Name "proc_main") ([], False)
                       [] Nothing 0 Nothing []

test_codegen = do
  withModuleFromAst simpleModule $ \m -> do
    withSystemTempDirectory "bsccTest." $ \tmpDirString -> do
      tmpDir <- Path.dynamicMakeAbsoluteFromCwd $ Path.absRel tmpDirString
      let objPath = tmpDir </> relFile "codegen.o"
      codegen m I686W64Mingw32 objPath
      fileCmdOutput <- readProcess "file" [Path.toString objPath] ""
      HU.assertBool ("Output from file: " ++ fileCmdOutput)
                    ("80386 COFF executable" `isInfixOf` fileCmdOutput)

test_withModuleFromAstDoesntThrow = do
  _ <- withModuleFromAst simpleModule return
  return ()

test_withModuleFromLlAsmStringDoesntThrow = do
  let llAsmStr = "define x86_stdcallcc void @proc_main()\n\
\{\n\
\  ret void\n\
\}\n\
\"
  _ <- withModuleFromLlAsmString llAsmStr return
  return ()
