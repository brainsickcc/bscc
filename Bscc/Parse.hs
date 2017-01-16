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

{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Parse our dialect of the BASIC language.  The parsers here work on
-- tokenized ("Bscc.Lex") input.
module Bscc.Parse (parseFileContents) where

import Bscc.Ast.Plain
import Bscc.Symbol.Name
import Bscc.Token

import Control.Applicative ((<*), (<*>), (*>), (<$>), pure)
import Prelude hiding (FilePath)
import System.Path (AbsFile, makeRelative, rootDir)
import qualified System.Path as Path
import Text.Parsec.Combinator (endBy, eof, sepEndBy, sepBy)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim ((<?>), parse, Parsec, skipMany, token)

-- GenParser and Parser follow a naming convention used in Parsec
-- itself.
type GenParser tok userState = Parsec [tok] userState
-- | The type of our parsers.  They consume `Token's.  A parser of type
-- @Parser Foo@ returns @Foo@ (in the `Either' monad) when successful.
type Parser = GenParser Token ()

-- | Parse a file which has already been lexed.  Returns an AST (lifted
-- into the `Either' monad) if successful.
parseFileContents :: [Token]      -- ^ Tokens, from lexing the file.
                     -> AbsFile
                     -> Either ParseError Module
parseFileContents tokens path =
  parse (basModule path) (Path.toString path) tokens


-- * Simple parsers, or general purpose parser combinators

-- | @commaSep p@ parses @p@ zero or more times, separated by
-- commas.
commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` (sym ',')

-- | Inputs a function which decides whether to accept a token.  Returns
-- a parser which consumes one token.
tokenAccepter :: (TokenNoPos -> Maybe a)  -- ^ This function should
                                          -- reject a token by returning
                                          -- @Nothing@, and accept it
                                          -- with return value @x@ by
                                          -- returning @Just x@.
                 -> Parser a
tokenAccepter testFn = token showToken posToken testToken
  where showToken (_pos, tok) = show tok
        posToken (pos, _tok) = pos
        testToken (_pos, tok) = testFn tok

-- | Parse an identifier.
ident :: Parser SymbolName
ident = tokenAccepter (\tok -> case tok of
                          TIdent name -> Just name
                          _ -> Nothing)
        <?> "identifier"

-- | Returns a parser for the input keyword.  Input keyword should be in
-- the canonical case.  For static safety guarantees, do not use this
-- function directly and instead use the kwFoo parsers.
kw :: String -> Parser ()
kw s = tokenAccepter (\tok -> case tok of
                         TKwCall | s == "Call" -> Just ()
                         TKwEnd | s == "End" -> Just ()
                         TKwSub | s == "Sub" -> Just ()
                         _ -> Nothing)
       <?> s

-- | /kwFoo/ parsers parse the keyword /Foo/.
kwCall, kwEnd, kwSub :: Parser ()
kwCall = kw "Call"
kwEnd = kw "End"
kwSub = kw "Sub"

-- | Parse a newline.
nl :: Parser ()
nl = tokenAccepter (\tok -> case tok of
                       TNl -> Just ()
                       _ -> Nothing)
     <?> "newline"

-- | Parse zero or more newlines.  Because such newlines are entirely
-- optional and indeed adding them will never help complete a parse,
-- this parser does not cause newline(s) to offered as a suggestion in
-- any error message.
nls :: Parser ()
nls = skipMany (nl <?> "")

-- | Parse one or more newlines.  Only the first newline is ever
-- recommended in any error message; it will never help a user to add
-- subsequent newlines to whatever they are trying to parse.
nls1 :: Parser ()
nls1 = nl *> nls

-- | @sepAndEndByNls1 p@ parses @p@ zero or more times, separated by one
-- or more newlines.  Additionally, parses one or more newlines after
-- the last @p@, if at least one @p@ was parsed.
sepAndEndByNls1 :: Parser a -> Parser [a]
sepAndEndByNls1 p = p `endBy` nls1

-- | @sepAndMaybeEndByNls1 p@ parses @p@ zero or more times, separated
-- by one or more newlines.  Additionally, optionally parses one or more
-- newlines (that is, necessarily parses zero or more newlines) after
-- the last @p@, if at least one @p@ was parsed.
sepAndMaybeEndByNls1 :: Parser a -> Parser [a]
sepAndMaybeEndByNls1 p = p `sepEndBy` nls1

-- | Parse a string literal.
stringLit :: Parser Expr
stringLit = tokenAccepter (\tok -> case tok of
                              TStringLit s -> Just $ StringLit s
                              _ -> Nothing)
            <?> "string literal"

-- | The returned parser accepts the corresponding input symbol.
sym :: Char -> Parser Char
sym c = tokenAccepter (\tok -> case tok of
                             TSym s | s == c -> Just c
                             _ -> Nothing)
           <?> [c]


-- * The interesting parsers for our grammar

-- | Returns a parser for the contents of the file.
basModule :: AbsFile         -- ^ Should be a \".bas\" file module.
           -> Parser Module
basModule path = BasModule (makeRelative rootDir path) <$>
                 (nls *>
                  sepAndMaybeEndByNls1 sub <*
                  eof)

-- | Parser for the Sub statement.  Subs are a kind of procedure.
sub :: Parser Proc
sub = Sub <$> (kwSub *> ident) <*> procArgsDef <* nls1
      <*> sepAndEndByNls1 inProcStmt
      <* (kwEnd <* kwSub <?> "End Sub")

-- | Parse the argument definition part of a procedure.  For example the
--
-- > ()
--
-- part of
--
-- > Function Foo() as Integer
--
-- For now we can only parse empty argument definitions.
procArgsDef :: Parser ArgDefs
procArgsDef = sym '(' *> sym ')' *> pure []

-- | Parser for statements which can appear inside a procedure.
inProcStmt :: Parser InProcStmt
inProcStmt = Call <$> (kwCall *> ident) <* sym '(' <*> commaSep expr <* sym ')'
             <?> "statement"

-- | Parse an expression.
expr :: Parser Expr
expr = stringLit <?> "expr"
