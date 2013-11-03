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

{-# LANGUAGE RankNTypes #-}

-- | Main (entry) file for the bscc program.
module Main (main) where

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
import Data.Maybe (fromJust)
import Data.Monoid ((<>), Last (getLast, Last), mappend, mempty, Monoid)
import qualified LLVM.General.AST as A
import qualified LLVM.General.Module as M
import Prelude hiding (readFile)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO.Temp (withSystemTempDirectory)
import System.Path (AbsFile,
                    asRelFile, getPathString, mkAbsPathFromCwd,
                    RelFile, replaceExtension, takeExtension,
                    (</>))
import System.Path.IO (readFile)

import Text.Groom (groom)

-- | Result of parsing our command line options, when in `Normal' mode.
data BsccOptions = BsccOptions { _outputName :: Last FilePath,
                                 _verbose :: Last Bool }
                   deriving (Show)

instance Monoid BsccOptions where
  mempty = BsccOptions { _outputName = mempty,
                         _verbose = mempty }
  x `mappend` y = BsccOptions { _outputName = _outputName x <> _outputName y,
                                _verbose = _verbose x <> _verbose y }

outputName :: L.Lens' BsccOptions (Last FilePath)
outputName = L.lens _outputName $ \s a -> s { _outputName = a }

verbose :: L.Lens' BsccOptions (Last Bool)
verbose = L.lens _verbose $ \s a -> s { _verbose = a }

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

doNormalMode :: BsccOptions -- ^ Parsed command line options.
                -> PosArgs  -- ^ List of files to compile.
                -> IO ()    -- ^ The returned computation performs
                            --   compilation.
doNormalMode options userFiles = do
  files <- mapM mkAbsPathFromCwd userFiles
  when (null files) $ fatalError "no input files"
  when (any ((/= ".bas") . takeExtension) files) $
    fatalError "files must be .bas files"
  let targetMachine = defaultTarget

  -- Lex and parse each of the files, and combine the result into one
  -- AST.
  perFileAsts <- forM files $ \path -> do
    contents <- readFile path
    tokens <- case lexFileContents contents path of
      Left parseError -> do
        errLn ("Encountered errors whilst lexing " ++ getPathString path ++
               ":")
        errLn $ show parseError
        exitFailure
      Right tokens -> return tokens
    case parseFileContents tokens path of
      Left parseError -> do
        errLn ("Encountered errors whilst parsing " ++ getPathString path ++
               ":")
        errLn $ show parseError
        exitFailure
      Right ast -> return ast
  let ast = Project perFileAsts $ mkSymbolName "Project1"
  when (options^.verbose & getLast & fromJust) $ do
    putStrLn "AST:"
    putStrLn . groom $ ast

  -- Perform semantic analysis.
  let typedAst = semAnalysis ast
  when (options^.verbose & getLast & fromJust) $ do
    putStrLn "\nSemantic analysis result:"
    putStrLn $ groom typedAst

  -- Generate the Intermediate Representation (IR), namely LLVM IR.
  let irAstAndPaths :: [(A.Module, RelFile)]
      irAstAndPaths = Ir.codegen typedAst
  when (options^.verbose & getLast & fromJust) $ do
    putStrLn "\nLLVM IR:"
    forM_ irAstAndPaths $ \(irAst, path) -> do
      putStrLn $ getPathString path ++ ":"
      content <- Mach.withModuleFromAst irAst M.moduleString
      putStr content

  -- A lot of the remainder of the compilation takes place in a temp dir.
  progName <- getProgName
  void $ withSystemTempDirectory (progName ++ ".") $ \tmpDirString -> do
    tmpDir <- mkAbsPathFromCwd tmpDirString
    -- libbsccts provides required startup code.
    libbscctsIrPath <- getDataFileName $ asRelFile "libbsccts/startup.ll"
    let libbscctsObjPath = tmpDir </> asRelFile "libbsccts-startup.ll"
    Mach.withModuleFromLlAsmFile libbscctsIrPath $ \mod' -> do
      Mach.codegen mod' targetMachine libbscctsObjPath

    mainObjPaths <- forM irAstAndPaths $ \(ir, irPath) -> do
      let objPathRel = (irPath `replaceExtension` ".o")
          objPath = tmpDir </> objPathRel
      Mach.withModuleFromAst ir $ \mod' -> do
        Mach.codegen mod' targetMachine objPath
        return objPath

    let objPaths :: [AbsFile]
        objPaths = libbscctsObjPath : mainObjPaths
    -- Link the object files into the executable.
    outputPath <- mkAbsPathFromCwd $ options^.outputName & getLast & fromJust
    link targetMachine objPaths outputPath

fatalError :: String -> IO ()
fatalError msg = do
  progName <- getProgName
  putStrLn $ progName ++ ": " ++ msg
  exitFailure
