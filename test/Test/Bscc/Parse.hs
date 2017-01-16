-- Copyright © 2013 Iain Nicol

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

module Test.Bscc.Parse (parseTests) where

import Bscc.Ast.Plain
import Bscc.Parse
import Bscc.Symbol.Name (mkSymbolName)
import Bscc.Token

import Control.Applicative (pure, (<$>), (<*>))
import System.Path (AbsFile)
import qualified System.Path as Path
import qualified Test.Tasty as T
import qualified Test.Tasty.QuickCheck as QC
import Text.Parsec.Error (ParseError)
import Text.Parsec.Pos (initialPos, SourcePos, updatePosChar)

parseTests =
  T.testGroup "Bscc.Parse"
  [
    QC.testProperty "extra newlines OK for empty sub"
                    prop_extraNewlinesOkForEmptySub,
    QC.testProperty "extra newlines OK for sub"
                    prop_extraNewlinesOkForSub
  ]

parse :: [TokenNoPos] -> Either ParseError Module
parse ts = parseFileContents (addDummyPositions ts) dummyFilename

addDummyPositions :: [TokenNoPos] -> [Token]
addDummyPositions ts = zip (positions ts) ts
  where positions ts' = scanl (incrementPos)
                              (initialPos $ Path.toString dummyFilename)
                              ts'
        incrementPos :: SourcePos -> TokenNoPos -> SourcePos
        incrementPos pos (TNl) = updatePosChar pos '\n'
        incrementPos pos _tok = updatePosChar pos 'a'

dummyFilename :: AbsFile
dummyFilename = (Path.absFile "fictitiousTestFile.bas")

moduleDoesParseTo :: [TokenNoPos] -> [Proc] -> Bool
moduleDoesParseTo ts expected = case parse ts of
  Left _ -> False
  Right (BasModule _ procs) -> procs == expected

genNls = QC.listOf (pure TNl)
genNls1 = QC.listOf1 (pure TNl)

mkIdent :: String -> TokenNoPos
mkIdent = TIdent . mkSymbolName

prop_extraNewlinesOkForEmptySub =
  QC.forAll ((,,) <$> genNls <*> genNls1 <*> genNls) $ \(n1, n2, n3) -> do
    (n1 ++
     [TKwSub, mkIdent "Main", TSym '(', TSym ')'] ++ n2 ++
     [TKwEnd, TKwSub] ++ n3)
    `moduleDoesParseTo`
    [Sub (mkSymbolName "Main") [] []]

-- Tests like this are far from pointless.  An early prototype used
-- uu-parsinglib.  Unfortunately, due to its correction support, it was
-- too picky about the amount of vertical whitespace.
prop_extraNewlinesOkForSub =
  QC.forAll ((,,,) <$> genNls <*> genNls1 <*> genNls1 <*> genNls) $
            \(n1, n2, n3, n4) -> do
    (n1 ++
     [TKwSub, mkIdent "Main", TSym '(', TSym ')'] ++ n2 ++
     [TKwCall, mkIdent "Foo", TSym '(', TSym ')'] ++ n3 ++
     [TKwEnd, TKwSub] ++ n4)
    `moduleDoesParseTo`
    [Sub (mkSymbolName "Main") []
         [Call (mkSymbolName "Foo") []]]
