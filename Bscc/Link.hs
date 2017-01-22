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

-- | Linker functionality.
module Bscc.Link (link) where

import qualified Bscc.Triplet as Triplet

import Control.Lens.Operators ((&))
import Control.Monad (void)
import Data.Char (isSpace)
import Prelude hiding (FilePath)
import System.Path (AbsFile)
import qualified System.Path as Path
import System.Process (readProcess)

-- | Determine the GCC version, e.g. "6.3.0".
gcc_version :: Triplet.Triplet -> IO String
gcc_version target = do
  let cmd = Triplet.str target ++ "-gcc"
  let stdin = []
  str <- readProcess cmd ["-dumpversion"] stdin
  return $ takeWhile (not . isSpace) str

-- | The returned computation links object files to produce an
-- executable.
link :: Triplet.Triplet  -- ^ Target machine triplet.
        -> [AbsFile]     -- ^ List of object file (\".o\") paths.
        -> AbsFile       -- ^ Path to write the executable to.
        -> IO AbsFile    -- ^ This computation performs the linking.
                         --   Its result is the path to the executable.
link target objFiles outputName = do
  gcc_version' <- gcc_version target
  let cmd = Triplet.str target ++ "-ld"
      libdir = "/usr/" ++ Triplet.str target ++ "/sys-root/mingw/lib"
      libdir_gcc = "/usr/lib/gcc/" ++ Triplet.str target ++ "/" ++ gcc_version'
      args = [["-o", Path.toString outputName,
               libdir ++ "/crt2.o",
               libdir_gcc ++ "/crtbegin.o"],
              -- Link errors unless objFiles come before -l.
              map Path.toString objFiles,
              -- Find and link against libvbstd.
              ["-L", "/usr/local/" ++ Triplet.str target ++
                     "/sys-root/mingw/lib/",
               "-lvbstd",
               -- MinGW GCC by default links against all of these
               -- libraries:
               "-L" ++ libdir_gcc,
               "-lgcc",
               "-lgcc_eh",
               "-L" ++ libdir,
               "-lmingw32",
               "-lmoldname",
               "-lmingwex",
               "-lmsvcrt",
               "-ladvapi32",
               "-lshell32",
               "-luser32",
               "-lkernel32",
               libdir_gcc ++ "/crtend.o",
               -- Use the GUI subsystem, as opposed to the default of
               -- console.
               "--subsystem=windows"]]
             & concat
      stdin = []
  void $ readProcess cmd args stdin
  return outputName
