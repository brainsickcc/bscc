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
module Bscc.HelpAndVersion (putHelp, putVersion) where

import Bscc.ThisPackage

import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Template (Context, substitute)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO

-- | The returned computation reads a textual template file, performs
-- the substitutions, and then prints that to stdout.
putFileWithSubsts :: FilePath   -- ^ Template ("Data.Text.Template")
                                --   file
                     -> Context -- ^ Substitutions to perform
                     -> IO ()
putFileWithSubsts path substs = do
  bs <- B.readFile path
  TIO.putStr $ substitute (decodeUtf8 bs) substs

-- | Substitutions to make to template files.
substs :: Context -- equivalently, T.Text -> T.Text
substs "bugsUrl" = T.pack bugsUrl
substs "homepage" = T.pack homepage
substs "version" = T.pack version
substs "copyright" = T.pack copyright

-- | Computation outputs a message to stdout, intended in response to
-- the --help command line argument.
putHelp :: IO ()
putHelp = do
  path <- getDataFileName "data/help.txt"
  putFileWithSubsts path substs

-- | Similar to `getHelp', but the message is in response to a --version
-- command line.
putVersion :: IO ()
putVersion = do
  path <- getDataFileName "data/version.txt"
  putFileWithSubsts path substs
