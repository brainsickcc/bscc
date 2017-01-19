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

-- | This module contains computations to respond to --help and
-- --version command line arguments.  It exists as its own module to
-- factor out code common to "Main" and bscc-help2man-shim.hs; this
-- keeps the dependencies of the latter as small as possible.
module Bscc.HelpAndVersion (
  helpMessage,
  versionMessage,
  putHelp,
  putVersion) where

-- Don't depend upon anything not in base.  bscc-help2man-shim cannot
-- depend upon any packages existing.  In particular, if bscc is built
-- in a sandbox, those sandbox packages will not be available to the
-- shim.
import Bscc.ThisPackage.Info

helpMessage :: String
helpMessage = "Usage: bscc [OPTION]... FILE...\n\
\Compiler for the classic, pre‐.NET, VB language\n\
\\n\
\Options:\n\
\  --help         Display a help message and exit\n\
\  --version      Display version information and exit\n\
\  -o <file>      Place the output into <file>\n\
\  -v             Display verbose information\n\
\\n\
\Report bugs to: <" ++ bugsUrl ++ ">\n\
\Home page: <" ++ homepage ++ ">"

versionMessage :: String
versionMessage = "bscc (Brainsick code compiler) " ++ version ++ "\n\
\\n\
\Copyright " ++ copyright ++ "\n\
\\n\
\License AGPLv3+: GNU Affero GPL version 3 or later <http://gnu.org/licenses/agpl.html>\n\
\This is free software: you are free to change and redistribute it.\n\
\There is NO WARRANTY, to the extent permitted by law."

-- | Computation outputs a message to stdout, intended in response to
-- the --help command line argument.
putHelp :: IO ()
putHelp = putStrLn helpMessage

-- | Similar to `getHelp', but the message is in response to a --version
-- command line.
putVersion :: IO ()
putVersion = putStrLn versionMessage
