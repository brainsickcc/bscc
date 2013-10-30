#!/usr/bin/env runhaskell

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

-- This file is compiled into a standalone executable by the ``cabal
-- configure'' command, and so must be called module Main and not Setup!

-- | Setup.hs is a standard file to have for projects using the Cabal
-- build system.
module Main (main) where

import Bscc.SetupHs (setupMain)

main :: IO ()
main = setupMain
