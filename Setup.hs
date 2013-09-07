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
-- build system.  Note however that this Setup.hs has dependencies
-- which, until installed, prevent building the build system; see the
-- INSTALL file.
module Main (main) where

import Distribution.Package (packageId)
import Distribution.PackageDescription (PackageDescription (package))
import Distribution.Simple (defaultMainWithHooks, hookedPrograms,
                            UserHooks (buildHook, instHook, postSDist))
import Distribution.Simple.Compiler (Compiler (compilerId))
import Distribution.Simple.InstallDirs (mandir, CopyDest (NoCopyDest))
import Distribution.Simple.LocalBuildInfo (absoluteInstallDirs,
                                           LocalBuildInfo (buildDir, compiler,
                                                           installDirTemplates,
                                                           withPrograms))
import Distribution.Simple.Program (defaultProgramConfiguration,
                                    getProgramOutput, locationPath, Program,
                                    programLocation, runProgram, simpleProgram)
import Distribution.Simple.Program.Db (requireProgram)
import Distribution.Simple.Setup (BuildFlags (buildVerbosity), fromFlag,
                                  flagToMaybe,
                                  InstallFlags (installVerbosity),
                                  SDistFlags (sDistDirectory, sDistDistPref,
                                              sDistVerbosity))
import Distribution.Simple.UUAGC (uuagcLibUserHook)
import Distribution.Simple.UserHooks (Args)
import Distribution.Simple.Utils (info, installOrdinaryFile)
import Distribution.Text (display)
import System.Directory (createDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))
import System.IO.Temp (withSystemTempDirectory)
import UU.UUAGC (uuagc)

-- | Entry point.  Largely this utility's functionality is stock-Cabal
-- functionality.  However, we override the basic behaviour by adding
-- hooks (see `myHooks').
main = defaultMainWithHooks myHooks

-- | Hooks to properly handle .ag files.  .ag files are compiled to .hs
-- files by /UUAGC/.
uuagcHooks :: UserHooks
uuagcHooks = uuagcLibUserHook uuagc

-- | Hooks to augment the stock-Cabal behaviour.  We support .ag files
-- with /UUAGC/.  Additionally, we build a man page using /help2man/.
-- Moreover, we generate the ChangeLog automatically when the source
-- distribution tarball is created.
myHooks :: UserHooks
myHooks = (uuagcHooks
           --Man page generation and installation.
           `appendHookedPrograms` [help2man, runhaskell]
           `appendBuildHook` manBuildHook
           `appendInstallHook` manInstallHook
           -- Replace placeholder ChangeLog contents with git log during
           -- sdist.
           `appendHookedPrograms` [git]
           `appendPostSDist` changeLogPostSDist)

-- For every program we define here (to be run at some point), we should
-- probably use with `appendHookedPrograms'; see that function's
-- documentation.
git, help2man, runhaskell :: Program
git = simpleProgram "git"
help2man = simpleProgram "help2man"
runhaskell = simpleProgram "runhaskell"

-- | Type of `buildHook' `UserHooks'.
type BuildHook = PackageDescription -> LocalBuildInfo -> UserHooks ->
                 BuildFlags -> IO ()
-- | Type of `instHook' `UserHooks'.
type InstallHook = PackageDescription -> LocalBuildInfo -> UserHooks ->
                   InstallFlags -> IO ()
-- | Type of `postSDist' `UserHooks'.
type PostSDist = Args -> SDistFlags -> PackageDescription ->
                     Maybe LocalBuildInfo -> IO ()

-- | Append a `BuildHook' to the `buildHook' of the `UserHooks'.
buildHook_a :: BuildHook -> UserHooks -> UserHooks
buildHook_a v r = r { buildHook = combinedBuildHook }
  where combinedBuildHook descr buildInfo hooks flags = do
          (buildHook r) descr buildInfo hooks flags
          v descr buildInfo hooks flags

-- | Append an `InstallHook' to the `instHook' of the `UserHooks'.
installHook_a :: InstallHook -> UserHooks -> UserHooks
installHook_a v r = r { instHook = combinedInstallHook }
  where combinedInstallHook descr buildInfo hooks flags = do
          (instHook r) descr buildInfo hooks flags
          v descr buildInfo hooks flags

-- | Append a `PostSDist' hook to the `postSDist' of the `UserHooks'.
-- `PostSDist' hook.
postSDist_a :: PostSDist -> UserHooks -> UserHooks
postSDist_a v r = r { postSDist = combinedSDistHook }
  where combinedSDistHook args flags packageDescr buildInfo = do
          (postSDist r) args flags packageDescr buildInfo
          v args flags packageDescr buildInfo

-- | Update the `hookedPrograms' field of the `UserHooks' by applying
-- the function to it.
hookedPrograms_u :: ([Program] -> [Program]) -> UserHooks -> UserHooks
hookedPrograms_u f r = r { hookedPrograms = f (hookedPrograms r) }

-- | `buildHook_a' with parameters flipped.
appendBuildHook = flip buildHook_a
-- | `installHook_a' with parameters flipped.
appendInstallHook = flip installHook_a
-- | `postSDist_a' with parameters flipped.
appendPostSDist = flip postSDist_a
-- | Append the programs to the `hookedPrograms' of the `UserHooks'.
-- This will cause the existence of these programs to be asserted at
-- ``cabal configure time.  Such programs should probably also be added
-- to to the build-tools section of our .cabal file.
appendHookedPrograms :: UserHooks -> [Program] -> UserHooks
appendHookedPrograms hooks progs = hookedPrograms_u (++ progs) hooks

-- | Create a man page using help2man.
manBuildHook :: BuildHook
manBuildHook descr buildInfo hooks flags = do
  let progsDb = withPrograms buildInfo
      verbosity = fromFlag $ buildVerbosity flags
      outFile = buildDir buildInfo </> "bscc.1"
  (help2man', _) <- requireProgram verbosity help2man progsDb
  -- For reasons documented in the shim program, a shim Haskell program
  -- is interpreted, and help2man uses this to generate the real
  -- bscc's man page.
  (runhaskell', _) <- requireProgram verbosity runhaskell progsDb
  let runhaskellPath = locationPath $ programLocation runhaskell'
  runProgram verbosity help2man' ["-o", outFile, "--no-info",
                                  runhaskellPath ++ " " ++
                                  "./bscc-help2man-shim.hs"]

-- | Install the man page.
manInstallHook :: InstallHook
manInstallHook descr buildInfo hooks flags = do
  let verbosity = fromFlag $ installVerbosity flags
      builtMan = buildDir buildInfo </> "bscc.1"
      resolvedManDir = mandir $ absoluteInstallDirs descr buildInfo NoCopyDest
      destMan1Dir = resolvedManDir </> "man1"
      destMan = destMan1Dir </> "bscc.1"
  createDirectoryIfMissing True destMan1Dir
  installOrdinaryFile verbosity builtMan destMan

-- | Ensures a real change log from the version control system ends up
-- in the distribution tarball.  Updates the to-be-tarballed directory
-- tree.
changeLogPostSDist :: PostSDist
changeLogPostSDist args flags packageDescr buildInfo = do
  let progsDb = (maybe defaultProgramConfiguration withPrograms buildInfo)
      verbosity = fromFlag $ sDistVerbosity flags
  info verbosity "Replacing dummy ChangeLog with the version control log"
  (git', _) <- requireProgram verbosity git progsDb
  let sDistDir = case flagToMaybe $ sDistDirectory flags of
        Just dir -> dir
        Nothing -> error "changeLogPostSDist: unknown sdist working directory"
  -- Make git format the log a bit like a GNU-style ChangeLog
  let gitLogParams = ["--date=short",
                      "--format=%ad  %an  <%ae>%n%n%w(80,8,8)%B"]
  changeLogContents <- getProgramOutput verbosity git' $
                       ["log"] ++ gitLogParams
  writeFile (sDistDir </> "ChangeLog") changeLogContents
