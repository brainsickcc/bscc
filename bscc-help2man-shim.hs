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

-- | This program impersonates the program /bscc/ enough for /help2man/
-- to generate a man page for bscc.  Having help2man run a built bscc
-- would introduce a barrier to cross compilation; instead, this program
-- is interpreted on the build machine by runhaskell.  In contrast to
-- the real bscc, this shim program has the advantage of fewer
-- dependencies.
module Main (main) where

import Bscc.HelpAndVersion (putHelp, putVersion)

import System.Environment (getArgs)

-- | Entry point.  Responds to a command line of \"--help\" or
-- \"version\" the same way bscc would.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> putHelp
    ["--version"] -> putVersion
    _ -> return ()
