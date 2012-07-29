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

-- | Symbol (identifier) names.
module Bscc.Symbol.Name (SymbolName, mkSymbolName, normalized, raw) where

import Data.Char (toLower, isAscii)

-- | Represents the name of a symbol (identifier).  Abstracts away the
-- case insensitivity.  \"Case insensitivity\" is quite vague, because
-- there are questions such as: /Which codepoints?  Under which locale?/
-- ASCII characters are case insensitive; any other case insensitivity
-- is possible but an implementation detail.
data SymbolName =
  SymbolName {
    -- | The normalized name.
    normalized :: String,
    -- | The raw name, for example as literally parsed.  For standard
    -- library exported symbols, this is the canonically cased name.
    raw :: String
    }
  deriving (Show)

instance Eq SymbolName where
  (SymbolName n1 _) == (SymbolName n2 _) = n1 == n2

instance Ord SymbolName where
  (SymbolName n1 _) <= (SymbolName n2 _) = n1 <= n2

-- | Create a `SymbolName', handling the case insensitivity.
--
-- When defining or declaring the symbols exported from the standard
-- library, pass in the symbol as canonically cased.  (This is to allow
-- the codegen to use the `raw' as opposed to `normalized' name, for
-- aesthetic reasons.)
mkSymbolName :: String -> SymbolName
mkSymbolName raw = SymbolName (map normalize raw) raw
   where normalize c = if isAscii c then toLower c else c
