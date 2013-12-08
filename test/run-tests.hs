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

-- The Haskell Test Framework preprocessor (htfpp) automagically
-- resolves the HTF test imports, and defines the function
-- `htf_importedTests'.
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- htfpp generates an unused `htf_Main_thisModulesTests'
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- | Test suite for bscc.  Usable by Cabal.
module Main (main) where

import {-@ HTF_TESTS @-} Test.Bscc.HelpAndVersion

import Test.Framework (makeTestSuite, TestSuite) -- Needed, blame htfpp
import Test.Framework.TestManager (htfMain)

-- | Runs the tests (HUnit Assertions and Testable QuickCheck
-- properties) visible to this module.  Exits with an error code if any
-- tests fail.
main :: IO ()
main = htfMain htf_importedTests
