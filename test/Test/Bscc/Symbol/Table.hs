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

module Test.Bscc.Symbol.Table (symbolTableTests) where

import Bscc.Ast.WithSem
import Bscc.Symbol.Name
import Bscc.Symbol.Table

import Prelude hiding (lookup)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU

symbolTableTests =
  T.testGroup "Bscc.Symbol.Table"
  [
    HU.testCase "can lookup inserted symbol case insensitive"
                test_canLookupInsertedSymbolCaseInsensitive,
    HU.testCase "later insert wins" test_laterInsertWins,
    HU.testCase "later insert wins regardless of case"
                test_laterInsertWinsRegardlessOfCase,
    HU.testCase "union is left biased" test_unionIsLeftBiased
  ]

symbolLowerXName, symbolUpperXName :: SymbolName
symbolLowerXName = mkSymbolName "x"
symbolUpperXName = mkSymbolName "X"

defineSymbol :: SymbolName -> SymbolName -> Symbol
defineSymbol symbol module' = SymbolProc proto
  where proto = SProcPrototype symbol [] Nothing
                (mkSymbolName "proj") module'

symbolLowerX, symbolUpperX1, symbolUpperX2 :: Symbol
symbolLowerX = defineSymbol symbolLowerXName (mkSymbolName "ModuleLower")
symbolUpperX1 = defineSymbol symbolUpperXName (mkSymbolName "ModuleUpper1")
symbolUpperX2 = defineSymbol symbolUpperXName (mkSymbolName "ModuleUpper2")

test_canLookupInsertedSymbolCaseInsensitive = do
  let table = insert symbolLowerX empty
      lookupRes = lookup symbolUpperXName table
  (Just symbolLowerX) HU.@=? lookupRes

test_laterInsertWins = do
  let table1 = insert symbolUpperX1 empty
      table2 = insert symbolUpperX2 table1
      lookupRes = lookup symbolUpperXName table2
  (Just symbolUpperX2) HU.@=? lookupRes

test_laterInsertWinsRegardlessOfCase = do
  let table1 = insert symbolLowerX empty
      table2 = insert symbolUpperX1 table1
      lookupRes = lookup symbolLowerXName table2
  (Just symbolUpperX1) HU.@=? lookupRes

test_unionIsLeftBiased = do
  let table1 = insert symbolUpperX1 empty
      table2 = insert symbolUpperX2 empty
      lookupRes = lookup symbolUpperXName (table1 `union` table2)
  (Just symbolUpperX1) HU.@=? lookupRes
