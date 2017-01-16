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

module Test.Bscc.Main (mainTests) where

import Bscc.Main (main)

import Control.Monad (void)
import Data.List (isInfixOf)
import Prelude hiding (writeFile)
import System.Environment (withArgs)
import System.IO.Temp (withSystemTempDirectory)
import System.Path (relFile, (</>))
import qualified System.Path as Path
import System.Path.IO (writeFile)
import System.Process (readProcess)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU

mainTests =
  T.testGroup "Bscc.Main"
  [
    HU.testCase "test hello compiles to exe" test_helloCompilesToExe
  ]

test_helloCompilesToExe = do
  void $ withSystemTempDirectory "bsccTest." $ \tmpDirString -> do
    tmpDir <- Path.dynamicMakeAbsoluteFromCwd $ Path.absRel tmpDirString
    let helloBasPath = tmpDir </> relFile "hello.bas"
    writeFile helloBasPath "Sub Main()\n\
\\n\
\    Call MsgBox(\"Hello, world!\")\n\
\\n\
\End Sub\n\
\"
    let helloExePath = tmpDir </> relFile "hello.exe"
    withArgs [Path.toString helloBasPath,
              "-o", Path.toString helloExePath] main
    fileCmdOutput <- readProcess "file" [Path.toString helloExePath] ""
    HU.assertBool ("Output from file: " ++ fileCmdOutput)
      ("PE32 executable (GUI) Intel 80386, for MS Windows"
       `isInfixOf` fileCmdOutput)
