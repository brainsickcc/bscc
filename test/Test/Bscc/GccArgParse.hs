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

module Test.Bscc.GccArgParse (gccArgParseTests) where

import Bscc.GccArgParse

import Data.Monoid (mempty, Monoid, Last (Last), mappend, (<>))
import qualified Control.Lens as L
import Control.Lens.Operators ((&), (.~))
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU

gccArgParseTests =
  T.testGroup "Bscc.GccArgParse"
  [
    HU.testCase "interspersed options and arguments"
                test_interspersedOptionsAndArguments,
    HU.testCase "help takes precedence over version"
                test_helpTakesPrecedenceOverVersion,
    HU.testCase "option argument hyphen hyphen"
                test_optionArgumentHyphenHyphen,
    HU.testCase "option argument hyphen hyphen help"
                test_optionArgumentHyphenHyphenHelp,
    HU.testCase "option argument equals foo"
                test_optionArgumentEqualsFoo,
    HU.testCase "option juxtaposition isn't a thing"
                test_optionJuxtapositionIsntAtThing,
    HU.testCase "option last wins" test_optionLastWins,
    HU.testCase "option squished param" test_optionSquishedParam,
    HU.testCase "option unsquished param" test_optionUnsquishedParam,
    HU.testCase "unknown option rejected" test_unknownOptionRejected
  ]

data OptionsResult = OptionsResult { _flagA :: Last String,
                                     _flagB :: Last Bool }
                   deriving (Eq, Show)

instance Monoid OptionsResult where
  mempty = OptionsResult { _flagA = mempty, _flagB = mempty }
  x `mappend` y = OptionsResult { _flagA = _flagA x <> _flagA y,
                                  _flagB = _flagB x <> _flagB y }

flagA :: L.Lens' OptionsResult (Last String)
flagA = L.lens _flagA $ \s a -> s { _flagA = a }

flagB :: L.Lens' OptionsResult (Last Bool)
flagB = L.lens _flagB $ \s a -> s { _flagB = a }

optionDecls :: [OptionDecl OptionsResult]
optionDecls = [optionADecl, optionBDecl]

optionADecl, optionBDecl :: OptionDecl OptionsResult
optionADecl = OptionDecl "-a" $ DeclOneSquishableArg (Last . Just) flagA
optionBDecl = OptionDecl "-b" $ DeclNoArgs (Last $ Just True) flagB

assertFailsToParse :: [UnparsedArg] -> HU.Assertion
assertFailsToParse args =
  case argsParse optionDecls args of
    Left _ -> HU.assertBool "" True
    Right res ->
      HU.assertBool ("Expecting parse failure but got: " ++ show res) False

assertParsesTo :: [UnparsedArg] -> Arguments OptionsResult -> HU.Assertion
assertParsesTo args expected =
  case argsParse optionDecls args of
    Left err -> case err of
      MissingArgumentToOption opt ->
        HU.assertBool ("missing parameter to option" ++ str opt) False
      UnrecognizedOption opt ->
        HU.assertBool ("unrecognized option " ++ opt) False
    Right res -> res HU.@?= expected

test_helpTakesPrecedenceOverVersion = do
  ["--help", "--version"] `assertParsesTo` Help
  ["--version", "--help"] `assertParsesTo` Help

test_interspersedOptionsAndArguments =
  ["alpha", "-b", "bravo", "-a", "aa", "-b", "charlie", "delta"]
  `assertParsesTo`
  Normal (mempty&flagA .~ (Last $ Just "aa")
                &flagB .~ (Last $ Just True))
         ["alpha", "bravo", "charlie", "delta"]

test_optionArgumentHyphenHyphen =
  ["-a", "--"] `assertParsesTo`
  Normal (mempty&flagA .~ (Last $ Just "--")) []

test_optionArgumentHyphenHyphenHelp =
  ["-a", "--help"] `assertParsesTo`
  Normal (mempty&flagA .~ (Last $ Just "--help")) []

test_optionArgumentEqualsFoo =
  ["-a=foo"] `assertParsesTo`
  Normal (mempty&flagA .~ (Last $ Just "=foo")) []

test_optionJuxtapositionIsntAtThing =
  -- Testing that this isn't interpreted like ["-a", "foo", "-b"].
  ["-ab", "foo"]
  `assertParsesTo`
  Normal (mempty&flagA .~ (Last $ Just "b")) ["foo"]

test_optionLastWins =
  ["-a", "foo", "-a", "bar"] `assertParsesTo`
  Normal (mempty&flagA .~ (Last $ Just "bar")) []

test_optionSquishedParam =
  ["-afoo"] `assertParsesTo`
  Normal (mempty&flagA .~ (Last $ Just "foo")) []

test_optionUnsquishedParam =
  ["-a", "foo"]  `assertParsesTo`
  Normal (mempty&flagA .~ (Last $ Just "foo")) []

test_unknownOptionRejected = assertFailsToParse ["-c"]
