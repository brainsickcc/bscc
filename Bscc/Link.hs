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

import System.Process (readProcess)

-- | The returned computation links object files to produce an
-- executable.
link :: Triplet.Triplet  -- ^ Target machine triplet.
        -> [FilePath]    -- ^ List of object file (\".o\") paths.
        -> FilePath      -- ^ Path to write the executable to.
        -> IO FilePath   -- ^ This computation performs the linking.
                         --   Its result is the path to the executable.
link target objFiles outputName = do
  -- We could use ld to do the linking, but instead we outsource the job
  -- to (mingw) gcc.  GCC takes care of knowing which low level support
  -- libraries or object code (might) be necessary to link against to
  -- successfully link objects for our target.  Pass gcc the "-v" flag
  -- to see what pain we're leaving for others to deal with.
  let cmd = Triplet.str target ++ "-gcc"
      args = objFiles -- Link errors unless objFiles come before -lbsa.
             ++ ["-o", outputName,
                 -- Find and link against libbsa.
                 "-L", "/usr/local/" ++ Triplet.str target ++
                       "/sys-root/mingw/lib/",
                 "-lbsa",
                 -- Use the GUI subsystem, as opposed to the default of
                 -- console.
                 "-Wl,--subsystem,windows"]
      stdin = []
  readProcess cmd args stdin
  return outputName
