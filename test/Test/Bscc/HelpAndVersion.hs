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
-- collects appropriately-named HUnit Assertions and Testable QuickCheck
-- properties, defining `htf_thisModulesTests'.
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Test.Bscc.HelpAndVersion (htf_thisModulesTests) where

import Bscc.HelpAndVersion (putHelp, putVersion)

import Data.List (isInfixOf)
import System.IO.Silently (capture)
import Test.Framework (
  -- .HUnitWrapper
  assertBool_,
  -- .Location
  makeLoc,
  -- .TestManager
  makeTestSuite, makeUnitTest, TestSuite
  )

-- | Tests the `putHelp' function outputs the expected information.
-- Implicitly checks that the underlying data file can be found and
-- read, and that all template substitutions are made.
test_putHelp = do
  (stdout, _) <- capture putHelp
  assertBool $ "Usage: " `isInfixOf` stdout
  assertBool $ "://www.brainsick.cc/" `isInfixOf` stdout

-- | Similar to `test_putHelp', but tests `putVersion'.
test_putVersion = do
  (stdout, _) <- capture putVersion
  -- ``--version'' should display the constant program name (as well as
  -- the varying program version).
  assertBool $ "Brainsick code compiler" `isInfixOf` stdout
