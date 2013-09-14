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

-- | Main (entry) file for the bscc program.
module Main (main) where

import Bscc.Ast.Plain (Project (..))
import qualified Bscc.Codegen.Ir as Ir
import qualified Bscc.Codegen.Machine as M
import Bscc.GccArgParse
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
import Control.Lens.Operators ((^.))
import Control.Monad (forM, forM_, void, when)
import System.Directory (copyFile)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.FilePath (takeExtension, (</>))
import System.IO (hPutStr, IOMode (WriteMode), withFile)
import System.IO.Temp (withSystemTempDirectory)
import Text.Groom (groom)

-- | Result of parsing our command line options, when in `Normal' mode.
data BsccOptions = BsccOptions { _outputName :: FilePath, _verbose :: Bool }
                   deriving (Show)
$(L.makeLenses ''BsccOptions)

-- | Represents successfully parsing zero command line options.
defaultOptions :: BsccOptions
defaultOptions = BsccOptions  { _outputName = "a.exe", _verbose = False }

-- | Command line options we declare and define our support for.
-- Options should be documented in data/help.txt so they appear in
-- --help output (and also in the man page).
myOptions :: [OptionDecl BsccOptions]
myOptions = [optionODecl, optionVDecl]

-- | Support for the output object name (\"-o\") option.
optionODecl :: OptionDecl BsccOptions
optionODecl = OptionDecl "-o" $ DeclOneSquishableArg (L.set outputName)

-- | Support for the verbose (\"-v\") option.
optionVDecl :: OptionDecl BsccOptions
optionVDecl = OptionDecl "-v" $ DeclNoArgs (L.set verbose True)

-- | Entry point.  Usually compile files, although as applicable handle
-- special command line arguments such as --help or --version.
main :: IO ()
main = do
  args <- getArgs
  case argsParse myOptions defaultOptions args of
    Help -> putHelp
    Version -> putVersion
    Normal options pos -> doNormalMode options pos

doNormalMode :: BsccOptions -- ^ Parsed command line options.
                -> PosArgs  -- ^ List of files to compile.
                -> IO ()    -- ^ The returned computation performs
                            --   compilation.
doNormalMode options userFiles = do
  when (null userFiles) $ error "no input files"
  when (any ((/= ".bas") . takeExtension) userFiles) $
    error "files must be .bas files"
  let targetMachine = defaultTarget

  -- Lex and parse each of the files, and combine the result into one
  -- AST.
  perFileAsts <- forM userFiles $ \path -> do
    contents <- readFile path
    tokens <- case lexFileContents contents path of
      Left parseError -> do
        errLn $ "Encountered errors whilst lexing " ++ path ++ ":"
        errLn $ show parseError
        exitFailure
      Right tokens -> return tokens
    case parseFileContents tokens path of
      Left parseError -> do
        errLn $ "Encountered errors whilst parsing " ++ path ++ ":"
        errLn $ show parseError
        exitFailure
      Right ast -> return ast
  let ast = Project perFileAsts $ mkSymbolName "Project1"
  when (options^.verbose) $ do
    putStrLn "AST:"
    putStrLn . groom $ ast

  -- Perform semantic analysis.
  let typedAst = semAnalysis ast
  when (options^.verbose) $ do
    putStrLn "\nSemantic analysis result:"
    putStrLn $ groom typedAst

  -- Generate the Intermediate Representation (IR), namely LLVM IR.
  let irAndPaths = Ir.codegen typedAst
  when (options^.verbose) $ do
    putStrLn "\nLLVM IR:"
    putStr $ unlines (map (\(content, path) -> path ++ ":\n" ++ content)
                      irAndPaths)

  -- A lot of the remainder of the compilation takes place in a temp dir.
  progName <- getProgName
  void $ withSystemTempDirectory (progName ++ ".") $ \tmpDir -> do
    -- Copy across the startup code every program requires.  This is
    -- also in LLVM IR.
    let libbscctsPath = tmpDir </> "libbsccts-startup.ll"
    origLibbscctsPath <- getDataFileName "libbsccts/startup.ll"
    copyFile origLibbscctsPath libbscctsPath
    -- Write the generate IR to files.  One file per input source file.
    forM_ irAndPaths $ \(ir, path) ->
      withFile (tmpDir </> path) WriteMode (`hPutStr` ir)

    -- Generate object files (with machine code for the target), for
    -- each LLVM IR file.
    let llPaths = map ((tmpDir </>) . snd) irAndPaths ++ [libbscctsPath]
    objPaths <- mapM (M.codegen targetMachine) llPaths

    -- Link the object files into the executable.
    link targetMachine objPaths $ options^.outputName
  return ()
