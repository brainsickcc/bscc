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

import Bscc.HelpAndVersion (helpMessage, versionMessage)

import Data.List (isInfixOf)
import qualified Data.Text as T
import Test.Framework (
  -- .HUnitWrapper
  assertBool_,
  -- .Location
  makeLoc,
  -- .TestManager
  makeTestSuite, makeUnitTest, TestSuite
  )

test_helpMessage = do
  assertBool $ "http://www.brainsick.cc" `isInfixOf` (T.unpack helpMessage)

test_versionMessage = do
  let hasCopyright = "Copyright" `isInfixOf` (T.unpack versionMessage)
      acksIain = "Iain Nicol" `isInfixOf` (T.unpack versionMessage)
  assertBool $ hasCopyright && acksIain
