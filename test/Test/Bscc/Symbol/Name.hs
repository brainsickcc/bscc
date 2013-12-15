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

module Test.Bscc.Symbol.Name (symbolNameTests) where

import Bscc.Symbol.Name

import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU

symbolNameTests =
  T.testGroup "Bscc.Symbol.Name"
  [
    HU.testCase "case insensitivity" test_caseInsensitivity,
    HU.testCase "inequality" test_inequality
  ]

test_caseInsensitivity = HU.assertEqual "" (mkSymbolName "sYmbOL")
                                           (mkSymbolName "SYmbol")

test_inequality = HU.assertBool "" $ (mkSymbolName "P") /= (mkSymbolName "NP")
