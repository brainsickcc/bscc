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

-- | Triplets are used to identify system types, such as the target
-- machine type.
module Bscc.Triplet (defaultTarget, cpu, str, Triplet (..)) where

-- | A triplet, as known from say cross compilation with Autotools and
-- GCC.
data Triplet = I686W64Mingw32 -- ^ 32-bit x86 Portable Executable (PE)

-- | Returns the CPU name portion of a triplet.
cpu :: Triplet -> String
cpu t = takeWhile (/= '-') (str t)

-- | Returns the canonical string representation of a triplet.
str :: Triplet -> String
str I686W64Mingw32 = "i686-w64-mingw32"

-- | The `Triplet' of our default target.  Currently our default target
-- is always 32-bit.
defaultTarget :: Triplet
defaultTarget = I686W64Mingw32
