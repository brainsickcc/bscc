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

module Test.Bscc.HelpAndVersion (helpAndVersionTests) where

import Bscc.HelpAndVersion (helpMessage, versionMessage)

import Data.List (isInfixOf)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU

helpAndVersionTests =
  T.testGroup "Bscc.HelpAndVersion"
  [
    HU.testCase "help message" test_helpMessage,
    HU.testCase "version message" test_versionMessage
  ]

test_helpMessage = do
  HU.assertBool "" $ "http://www.brainsick.cc" `isInfixOf` helpMessage

test_versionMessage = do
  let hasCopyright = "Copyright" `isInfixOf` versionMessage
      acksIain = "Iain Nicol" `isInfixOf` versionMessage
  HU.assertBool "" $ hasCopyright && acksIain
