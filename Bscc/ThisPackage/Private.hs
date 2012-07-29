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

{-# LANGUAGE FlexibleInstances #-}

-- | This module is a private implementation detail of
-- "Bscc.ThisPackage".  It is used for an instance it defines, which
-- because of a limitation of Template Haskell cannot reside directly in
-- "Bscc.ThisPackage".
module Bscc.ThisPackage.Private () where

import Distribution.Compat.ReadP (many, satisfy)
import qualified Distribution.Text as T
import Text.PrettyPrint (text)

-- | Make String an instance of Distribution.Text.Text.  This allows
-- Bscc.ThisPackage to use cabal-file-th's packageVariable to pull out
-- raw strings from our .cabal file.
instance T.Text String where
    disp = text
    parse = many $ satisfy (const True)
