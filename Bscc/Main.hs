
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

{-# LANGUAGE TemplateHaskell #-}

-- | Implements the entry procedure of the bscc program.
module Bscc.Main (main) where

import Bscc.Ast.Plain (Project (..))
import qualified Bscc.Codegen.Ir as Ir
import qualified Bscc.Codegen.Machine as Mach
import Bscc.GccArgParse as Arg
import Bscc.HelpAndVersion
import Bscc.Lex
import Bscc.Link
import Bscc.Parse
import Bscc.Sem
import Bscc.Symbol.Name (mkSymbolName)
import Bscc.ThisPackage.Dir (getDataFileName)
import Bscc.Triplet

import Control.Error.Util (errLn)
import qualified Control.Lens as L
import Control.Lens.Operators ((&), (^.))
import Control.Monad (forM, forM_, void, when)
import Data.Foldable (mapM_)
import Data.Maybe (fromJust)
import Data.Monoid ((<>), Last (getLast, Last), mappend, mempty, Monoid)
import qualified LLVM.General.AST as A
import qualified LLVM.General.Module as M
import Prelude hiding (mapM_, readFile)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (exitFailure)
import System.IO.Temp (withSystemTempDirectory)
import System.Path (AbsFile,
                    relFile,
                    RelFile, replaceExtension, takeExtension,
                    (</>))
import qualified System.Path.Generic as Path
import System.Path.IO (readFile)

import Text.Groom (groom)

-- | Result of parsing our command line options, when in `Normal' mode.
data BsccOptions = BsccOptions { _outputName :: Last FilePath,
                                 _verbose :: Last Bool }
                   deriving (Show)
$(L.makeLenses ''BsccOptions)

instance Monoid BsccOptions where
  mempty = BsccOptions { _outputName = mempty,
                         _verbose = mempty }
  x `mappend` y = BsccOptions { _outputName = _outputName x <> _outputName y,
                                _verbose = _verbose x <> _verbose y }

-- | Represents successfully parsing zero command line options.
defaultOptions :: BsccOptions
defaultOptions = BsccOptions  { _outputName = Last $ Just "a.exe",
                                _verbose = Last $ Just False }

-- | Command line options we declare and define our support for.
-- Options should be documented in data/help.txt so they appear in
-- --help output (and also in the man page).
myOptions :: [OptionDecl BsccOptions]
myOptions = [optionODecl, optionVDecl]

-- | Support for the output object name (\"-o\") option.
optionODecl :: OptionDecl BsccOptions
optionODecl = OptionDecl "-o" $ DeclOneSquishableArg (Last . Just) outputName

-- | Support for the verbose (\"-v\") option.
optionVDecl :: OptionDecl BsccOptions
optionVDecl = OptionDecl "-v" $ DeclNoArgs (Last $ Just True) verbose

-- | Entry point.  Usually compile files, although as applicable handle
-- special command line arguments such as --help or --version.
main :: IO ()
main = do
  args <- getArgs
  case argsParse myOptions args of
    Left err -> case err of
      MissingArgumentToOption opt ->
        error $ "missing parameter to '" ++ Arg.str opt ++ "'"
      UnrecognizedOption opt -> fatalError $ "unrecognised option: " ++ opt
    Right res -> case res of
      Help -> putHelp
      Version -> putVersion
      Normal options pos -> doNormalMode (defaultOptions <> options) pos

-- | Turns relative paths into absolute paths, and leaves absolute paths
-- as-is.
--
-- If running as an AppImage this function will use the original current
-- working directory.  That makes this suitable for resolving user
-- specified paths, as opposed to paths for files which are part of this
-- application's packaging.
absolutizePath :: Path.System os =>
                  Path.AbsRel os fd -> IO (Path.Abs os fd)
absolutizePath path = do
  owd <- lookupEnv "OWD"
  case owd of
    Just dir -> return $ Path.dynamicMakeAbsolute (Path.abs dir) path
    Nothing -> Path.dynamicMakeAbsoluteFromCwd path

doNormalMode :: BsccOptions -- ^ Parsed command line options.
                -> PosArgs  -- ^ List of files to compile.
                -> IO ()    -- ^ The returned computation performs
                            --   compilation.
doNormalMode options userFiles = do
  files <- mapM (absolutizePath . Path.absRel) userFiles
  when (null files) $ fatalError "no input files"
  let targetMachine = defaultTarget

  -- Lex and parse each of the files, and combine the result into one
  -- AST.
  perFileAsts <- forM files $ \path -> do
    contents <- readFile path
    tokens <- case lexFileContents contents path of
      Left parseError -> do
        errLn ("Encountered errors whilst lexing " ++ Path.toString path ++
               ":")
        errLn $ show parseError
        exitFailure
      Right tokens -> return tokens
    case parseFileContents tokens path of
      Left parseError -> do
        errLn ("Encountered errors whilst parsing " ++ Path.toString path ++
               ":")
        errLn $ show parseError
        exitFailure
      Right ast -> return ast
  let ast = Project perFileAsts $ mkSymbolName "Project1"
  -- when (options^.verbose & getLast & fromJust) $ do
  --   putStrLn "AST:"
  --   putStrLn . groom $ ast

  -- Perform semantic analysis.
  typedAst <- case semAnalysis ast of
    Left errs -> do
      errLn $ "Encountered errors during semantic analysis:"
      mapM_ print errs
      exitFailure
    Right typedAst -> return typedAst
  -- when (options^.verbose & getLast & fromJust) $ do
  --   putStrLn "\nSemantic analysis result:"
  --   putStrLn $ groom typedAst

  -- Generate the Intermediate Representation (IR), namely LLVM IR.
  let irAstAndPaths :: [(A.Module, RelFile)]
      irAstAndPaths = Ir.codegen typedAst
  when (options^.verbose & getLast & fromJust) $ do
    putStrLn "\nLLVM IR:"
    forM_ irAstAndPaths $ \(irAst, path) -> do
      putStrLn $ Path.toString path ++ ":"
      content <- Mach.withModuleFromAst irAst M.moduleLLVMAssembly
      putStr content

  -- A lot of the remainder of the compilation takes place in a temp dir.
  progName <- getProgName
  void $ withSystemTempDirectory (progName ++ ".") $ \tmpDirString -> do
  -- void $ do
    -- let tmpDirString = "/home/iain/programming/brainsickref/compilation-tmp"
    tmpDir <- absolutizePath $ Path.absRel tmpDirString
    -- libbsccts provides required startup code.
    libbscctsIrPath <- getDataFileName $ relFile "libbsccts/startup.ll"
    let libbscctsObjPath = tmpDir </> relFile "libbsccts-startup.ll"
    Mach.withModuleFromLlAsmFile libbscctsIrPath $ \mod' -> do
      Mach.codegen mod' targetMachine libbscctsObjPath

    --libbscctsIrPath2 <- getDataFileName $ relFile "libbsccts/form-user.ll"
    --let libbscctsObjPath2 = tmpDir </> relFile "libbsccts-form-user.ll"
    --Mach.withModuleFromLlAsmFile libbscctsIrPath2 $ \mod' -> do
    --  Mach.codegen mod' targetMachine libbscctsObjPath2

    mainObjPaths <- forM irAstAndPaths $ \(ir, irPath) -> do
      let objPathRel = (irPath `replaceExtension` ".o")
          objPath = tmpDir </> objPathRel
      Mach.withModuleFromAst ir $ \mod' -> do
        Mach.codegen mod' targetMachine objPath
        return objPath

    let objPaths :: [AbsFile]
        objPaths = libbscctsObjPath : mainObjPaths
        --objPaths = libbscctsObjPath : libbscctsObjPath2 : mainObjPaths
    -- Link the object files into the executable.
    outputPath <- absolutizePath $ Path.absRel $
      options^.outputName & getLast & fromJust
    link targetMachine objPaths outputPath

fatalError :: String -> IO ()
fatalError msg = do
  progName <- getProgName
  putStrLn $ progName ++ ": " ++ msg
  exitFailure
