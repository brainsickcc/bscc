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

module Test.Bscc.Lex (lexTests) where

import Bscc.Lex
import Bscc.Symbol.Name
import Bscc.Token

import Prelude hiding (lex)
import System.Path (AbsFile, asAbsFile)
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC
import Text.Parsec.Error (ParseError)

lexTests =
  T.testGroup "Bscc.Lex"
  [
    QC.testProperty "extra spaces ok" prop_extraSpacesOk,
    HU.testCase "keyword" test_keyword,
    HU.testCase "keyword case insensitive" test_keyword_caseInsenstive,
    HU.testCase "keyword suffix" test_keywordSuffix,
    HU.testCase "newline lf" test_newlineLf,
    HU.testCase "newline lf lf" test_newlineLfLf,
    HU.testCase "partial success is failure" test_partialSuccessIsFailure,
    HU.testCase "string empty" test_stringEmpty,
    HU.testCase "string two embedded quotes" test_stringTwoEmbeddedQuotes,
    HU.testCase "string with embedded quote" test_stringWithEmbeddedQuote,
    HU.testCase "reject invalid punctuation" test_rejectInvalidPunctuation
  ]

dummyFilename :: AbsFile
dummyFilename = (asAbsFile "fictitiousTestFile.bas")

lex :: String -> Either ParseError [Token]
lex str = lexFileContents str dummyFilename

lexFailureMsg :: String -> String
lexFailureMsg lexInput =
  "Can't test lex output because the input wouldn't lex: " ++
  show lexInput

assertFailsLex :: String -> HU.Assertion
assertFailsLex s = HU.assertBool "" $ either (const True) (const False) (lex s)

-- Assert both successfully lex, to the same tokens (ignoring positions).
assertLexesSame :: String -> String -> HU.Assertion
assertLexesSame x y = do
  x' <- getLexSuccess x
  y' <- getLexSuccess y
  HU.assertEqual "" x' y'
  where getLexSuccess str = case lex str of
          Left _ -> error $ lexFailureMsg x
          Right ts -> return ts

assertLexesTo :: String -> [TokenNoPos] -> HU.Assertion
assertLexesTo str expected = case lex str of
  Left _ -> HU.assertFailure $ lexFailureMsg str
  Right lexRes -> lexResNoPosition HU.@?= expected
    where lexResNoPosition = map snd lexRes

genSpaces1 = QC.listOf1 (return ' ')

doesLexTo :: String -> [TokenNoPos] -> Bool
doesLexTo str expected = case lex str of
  Left _ -> False
  Right lexRes -> lexResNoPosition == expected
    where lexResNoPosition = map snd lexRes

-- | Invalid for top level lexing.  Only valid inside a string, comment,
-- or [quoted identifier].
invalidPunct :: Char
invalidPunct = '"'

prop_extraSpacesOk = QC.forAll genSpaces1 $ \spaces ->
  ("End" ++ spaces ++ "Sub") `doesLexTo` [TKwEnd, TKwSub]

test_keyword = "Call" `assertLexesTo` [TKwCall]
test_keywordSuffix = "Callx" `assertLexesTo` [TIdent $ mkSymbolName "Callx"]
test_keyword_caseInsenstive = "call" `assertLexesSame` "cAlL"

test_newlineLf = "\n" `assertLexesTo` [TNl]
test_newlineLfLf = "\n\n" `assertLexesTo` [TNl, TNl]

test_partialSuccessIsFailure = assertFailsLex "Call |"

beginStr, endStr :: Char
beginStr = '"'
endStr = '"'

test_stringEmpty = [beginStr, endStr] `assertLexesTo` [TStringLit ""]
test_stringTwoEmbeddedQuotes = [beginStr,
                                '"', '"',
                                '"', '"',
                                endStr]
                               `assertLexesTo`
                               [TStringLit ['"', '"']]
test_stringWithEmbeddedQuote = [beginStr,
                                'a',
                                '"', '"',
                                'b',
                                endStr]
                                `assertLexesTo`
                               [TStringLit ['a', '"', 'b']]

test_rejectInvalidPunctuation = assertFailsLex [invalidPunct]
