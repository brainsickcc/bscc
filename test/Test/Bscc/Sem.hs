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

module Test.Bscc.Sem (semTests) where

import Bscc.Sem
import Bscc.Ast.Plain
import Bscc.Ast.WithSem
import Bscc.Symbol.Name

import System.Path (relFile, RelFile)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU

semTests =
  T.testGroup "Bscc.Sem"
  [
    HU.testCase "call needs correct arg types" test_callNeedsCorrectArgTypes,
    HU.testCase "call needs correct num args" test_callNeedsCorrectNumArgs,
    HU.testCase "call needs target" test_callNeedsTarget,
    HU.testCase "call can resolve target in another module"
                test_callCanResolveTargetInAnotherModule,
    HU.testCase ("call resolves with preference to local module regardless of "
                 ++ "case")
                test_callResolvesWithPreferenceToLocalModuleRegardlessOfCase,
    HU.testCase "duplicate function name at module scope disallowed"
                test_duplicateFunctionNameAtModuleScopeDisallowed,
    HU.testCase "need program entry point" test_needProgramEntryPoint,
    HU.testCase "Sub Main duplicate disallowed"
                test_subMainDuplicateDisallowed,
    HU.testCase "Sub Main one between multiple modules is good"
                test_subMainOneBetweenMultipleModulesIsGood
  ]

assertFailsSem :: Project -> HU.Assertion
assertFailsSem x = case semAnalysis x of
  Left _errs -> return ()
  Right proj -> HU.assertFailure $
                "Expecting Sem failure, but Sem was successful and returned: "
                ++ show proj

assertPassesSem :: Project -> HU.Assertion
assertPassesSem x = case semAnalysis x of
  Left errs -> HU.assertFailure $
                "Expecting Sem pass, but Sem failed with errs: " ++ show errs
  Right _proj -> return ()

assertSemsTo :: Project -> SProject -> HU.Assertion
assertSemsTo input expected = case semAnalysis input of
  Left errs -> HU.assertFailure $
               "Expecting Sem pass, but failed with errs: " ++ show errs
  Right proj -> proj HU.@?= expected

emptySubMain = Sub (mkSymbolName "Main") [] []

module1, module2 :: SymbolName
module1 = mkSymbolName "Module1"
module2 = mkSymbolName "Module2"

module1Path, module2Path :: RelFile
module1Path = (relFile "Module1.bas")
module2Path = (relFile "Module2.bas")

project1 :: SymbolName
project1 = mkSymbolName "Project1"

test_callNeedsCorrectArgTypes = assertFailsSem proj
  where proj = Project [BasModule module1Path [subMain, subTarget]] project1
        subMain = Sub (mkSymbolName "Main") []
                      [Call (mkSymbolName "Target") [StringLit "s"]]
        subTarget = Sub (mkSymbolName "Target")
                        [ArgDef (mkSymbolName "X") DummySecondType]
                        []

test_callNeedsCorrectNumArgs = assertFailsSem proj
  where proj = Project [BasModule module1Path [subMain, subTarget]] project1
        subMain = Sub (mkSymbolName "Main") []
                      [Call (mkSymbolName "Target") [StringLit "s"]]
        subTarget = Sub (mkSymbolName "Target") [] []

test_callNeedsTarget = assertFailsSem proj
  where proj = Project [BasModule module1Path [emptySubMain, subTargetless]]
                       project1
        subTargetless = Sub (mkSymbolName "Targetless") []
                            [Call (mkSymbolName "NoSuchTarget") []]

test_callCanResolveTargetInAnotherModule = input `assertSemsTo` expected
  where input = Project [BasModule module1Path [subMainIn],
                         BasModule module2Path [subTargetIn]]
                        project1
        subMainIn = Sub (mkSymbolName "Main") []
                      [Call (mkSymbolName "Target") []]
        subTargetIn = Sub (mkSymbolName "Target") [] []
        expected = SProject [SBasModule module1Path [subMainOut] module1,
                             SBasModule module2Path [subTargetOut] module2]
                            project1
        subMainOut = SProc (SProcPrototype (mkSymbolName "Main") [] Nothing
                                           project1 module1)
                           [SCall targetProto []]
        subTargetOut = SProc targetProto []
        targetProto = SProcPrototype (mkSymbolName "Target") [] Nothing
                                     project1 module2

test_callResolvesWithPreferenceToLocalModuleRegardlessOfCase =
  input `assertSemsTo` expected
  where input = Project [BasModule module1Path [subMainIn,
                                                subTargetWeirdCaseIn],
                         BasModule module2Path [subTargetCanonCaseIn]]
                        project1
        subMainIn = Sub (mkSymbolName "Main") []
                      [Call (mkSymbolName "Target") []]
        subTargetWeirdCaseIn = Sub (mkSymbolName "TaRgeT") [] []
        subTargetCanonCaseIn = Sub (mkSymbolName "Target") [] []
        expected = SProject [SBasModule module1Path
                                        [subMainOut, subTargetWeirdCaseOut]
                                        module1,
                             SBasModule module2Path
                                        [subTargetCanonCaseOut]
                                        module2]
                            project1
        subMainOut = SProc (SProcPrototype (mkSymbolName "Main") [] Nothing
                                           project1 module1)
                           [SCall subTargetWeirdCaseProto []]
        subTargetWeirdCaseProto =
          (SProcPrototype (mkSymbolName "TaRgeT") [] Nothing project1 module1)
        subTargetWeirdCaseOut = SProc subTargetWeirdCaseProto []
        subTargetCanonCaseOut =
          SProc (SProcPrototype (mkSymbolName "Target") [] Nothing
                                project1 module2)
                []

test_duplicateFunctionNameAtModuleScopeDisallowed = assertFailsSem proj
  where proj = Project [BasModule module1Path [emptySubMain, emptySubMain]]
                       project1

-- Can't have two Sub Mains, even if one were a Private Sub!
test_subMainDuplicateDisallowed = assertFailsSem proj
  where proj = Project [BasModule module1Path [emptySubMain],
                        BasModule module2Path [emptySubMain]]
                       project1

test_subMainOneBetweenMultipleModulesIsGood = assertPassesSem proj
  where proj = Project [BasModule module1Path [emptySubMain],
                        BasModule module2Path []]
                       project1

test_needProgramEntryPoint = assertFailsSem proj
  where proj = Project [BasModule module1Path [subNotMain]] project1
        subNotMain = Sub (mkSymbolName "NotMain") [] []
