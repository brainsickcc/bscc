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

{-# LANGUAGE OverloadedStrings #-}

-- | This module contains computations to respond to --help and
-- --version command line arguments.  It exists as its own module to
-- factor out code common to "Main" and bscc-help2man-shim.hs; this
-- keeps the dependencies of the latter as small as possible.
module Bscc.HelpAndVersion (
  helpMessage,
  versionMessage,
  putHelp,
  putVersion) where

import Bscc.ThisPackage

import Data.Text.Template (Context, substitute)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as TIO

-- | Substitutions to make to template files.
substs :: Context -- equivalently, T.Text -> T.Text
substs "bugsUrl" = T.pack bugsUrl
substs "homepage" = T.pack homepage
substs "version" = T.pack version
substs "copyright" = T.pack copyright

helpMessageTemplate :: T.Text
helpMessageTemplate = T.pack "Usage: bscc [OPTION]... FILE...\n\
\Compile .bas FILE(s) written in the ``Brainsick'' BASIC dialect\n\
\\n\
\Options:\n\
\  --help         Display a help message and exit\n\
\  --version      Display version information and exit\n\
\  -o <file>      Place the output into <file>\n\
\  -v             Display verbose information\n\
\\n\
\Report bugs to: <$bugsUrl>\n\
\Home page: <$homepage>"

helpMessage :: T.Text
helpMessage = L.toStrict $ substitute helpMessageTemplate substs

versionMessageTemplate :: T.Text
versionMessageTemplate = T.pack "bscc (Brainsick code compiler) $version\n\
\\n\
\Copyright $copyright\n\
\\n\
\License AGPLv3+: GNU Affero GPL version 3 or later <http://gnu.org/licenses/agpl.html>\n\
\This is free software: you are free to change and redistribute it.\n\
\There is NO WARRANTY, to the extent permitted by law."

versionMessage :: T.Text
versionMessage = L.toStrict $ substitute versionMessageTemplate substs

-- | Computation outputs a message to stdout, intended in response to
-- the --help command line argument.
putHelp :: IO ()
putHelp = TIO.putStrLn helpMessage

-- | Similar to `getHelp', but the message is in response to a --version
-- command line.
putVersion :: IO ()
putVersion = TIO.putStrLn versionMessage
