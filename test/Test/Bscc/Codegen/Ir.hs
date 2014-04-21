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

module Test.Bscc.Codegen.Ir (codegenIrTests) where

import Bscc.Ast.WithSem
import Bscc.Codegen.Machine (withModuleFromAst, withModuleFromLlAsmString)
import Bscc.Codegen.Ir
import Bscc.Symbol.Name (mkSymbolName, SymbolName)

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad ((>=>))
import qualified LLVM.General.AST as A
import qualified LLVM.General.Module as M
import System.Path (asRelFile, RelFile)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU

codegenIrTests =
  T.testGroup "Bscc.Codegen.Ir"
  [
    HU.testCase "calling procedure defined in another module"
                test_callingProcedureDefinedInAnotherModule,
    HU.testCase "calling procedure defined in same module"
                test_callingProcedureDefinedInSameModule,
    HU.testCase "calling procedure with string param"
                test_callingProcedureWithStringParam
  ]

-- | Asserts that the result of codegen-ing the first parameter is
-- equivalent to the [(LLVM assembly, filename)] given in the second
-- parameter.
assertCodegensTo :: SProject -> [(String, RelFile)] -> HU.Assertion
assertCodegensTo proj expected = do
  let pureAstToAsm :: (A.Module, RelFile) -> IO (String, RelFile)
      pureAstToAsm (ast, f) = (,)
                              <$> withModuleFromAst ast M.moduleLLVMAssembly
                              <*> pure f
  let asmToPureAst :: (String, RelFile) -> IO (A.Module, RelFile)
      asmToPureAst (asm, f) = (,)
                              <$> withModuleFromLlAsmString asm M.moduleAST
                              <*> pure f
  expected' <- mapM asmToPureAst expected
  let actual :: [(A.Module, RelFile)]
      actual = codegen proj
  -- Need to undergo normalization.  Otherwise the UnNames (from
  -- llvm-general-pure) between the actual and the expected result might
  -- unnecessarily differ in numbering.
  actualNormalized <- mapM (pureAstToAsm >=> asmToPureAst) actual
  actualNormalized HU.@?= expected'

main :: SymbolName
main = mkSymbolName "Main"

module1, module2 :: SymbolName
module1 = mkSymbolName "Module1"
module2 = mkSymbolName "Module2"

module1Path, module2Path :: RelFile
module1Path = (asRelFile "Module1.bas")
module2Path = (asRelFile "Module2.bas")

project1 :: SymbolName
project1 = mkSymbolName "Project1"

target :: SymbolName
target = mkSymbolName "target"

test_callingProcedureDefinedInAnotherModule =
  ast `assertCodegensTo` [(asm1, asRelFile "Module1.ll"),
                          (asm2, asRelFile "Module2.ll")]
  where ast = SProject [SBasModule module1Path [subMain] module1,
                        SBasModule module2Path [subTarget] module2]
                       project1
        subMain = SProc (SProcPrototype main [] Nothing
                                        project1 module1)
                        [SCall targetProto []]
        subTarget = SProc targetProto []
        targetProto = SProcPrototype target [] Nothing project1 module2
        asm1 = ("declare x86_stdcallcc void @proc_target()\n" ++
                "\n" ++
                "define x86_stdcallcc void @proc_main() {\n" ++
                "  call x86_stdcallcc void @proc_target()\n" ++
                "  ret void\n" ++
                "}\n")
        asm2 = ("define x86_stdcallcc void @proc_target() {\n" ++
                "  ret void\n" ++
                "}\n")

test_callingProcedureDefinedInSameModule =
  ast `assertCodegensTo` [(asm, asRelFile "Module1.ll")]
  where ast = SProject [SBasModule module1Path [subMain, subTarget] module1]
                       project1
        subMain = SProc (SProcPrototype main [] Nothing project1 module1)
                        [SCall targetProto []]
        subTarget = SProc targetProto []
        targetProto = SProcPrototype target [] Nothing project1 module1
        asm = ("define x86_stdcallcc void @proc_main() {\n" ++
               "  call x86_stdcallcc void @proc_target()\n" ++
               "  ret void\n" ++
               "}\n" ++
               "\n" ++
               "define x86_stdcallcc void @proc_target() {\n" ++
               "  ret void\n" ++
               "}\n")

test_callingProcedureWithStringParam =
  ast `assertCodegensTo` [(asm, asRelFile "Module1.ll")]
  where
    ast = SProject [SBasModule module1Path [subMain, subTarget] module1]
                   project1
    subMain = SProc (SProcPrototype main [] Nothing project1 module1)
                    [SCall targetProto [SStringLit "Hello" SString]]
    subTarget = SProc targetProto []
    targetProto = SProcPrototype target
                                 [SArgDef (mkSymbolName "X") SString] Nothing
                                 project1 module1
    asm = (
      "@s1 = private constant [6 x i16] [i16 72, i16 101, i16 108,\n" ++
      "                                  i16 108, i16 111, i16 0]\n" ++
      "\n" ++
      "define x86_stdcallcc void @proc_main() {\n" ++
      "  call x86_stdcallcc void @proc_target(i16* getelementptr inbounds\n" ++
      "      ([6 x i16]* @s1, i64 0, i64 0))\n" ++
      "  ret void\n" ++
      "}\n" ++
      "\n" ++
      "define x86_stdcallcc void @proc_target(i16*) {\n" ++
      "  ret void\n" ++
      "}\n")
