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

-- | Test suite for bscc.  Usable by Cabal.
module Main (main) where

import Test.Bscc.HelpAndVersion

import Test.Tasty (defaultMain, testGroup, TestTree)

-- | Runs the tests (HUnit Assertions and Testable QuickCheck
-- properties) visible to this module.  Exits with an error code if any
-- tests fail.
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Bscc"
  [
    helpAndVersionTests
  ]
