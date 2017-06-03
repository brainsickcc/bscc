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

-- | Parse the VB language.  The parsers here work on tokenized
-- ("Bscc.Lex") input.
module Bscc.Parse (parseFileContents) where

import Bscc.Ast.Plain
import Bscc.Symbol.Name
import Bscc.Token

import Control.Applicative ((<*), (<*>), (*>), (<$>), pure)
import Prelude hiding (FilePath)
import System.Path (AbsFile, makeRelative, rootDir)
import qualified System.Path as Path
import Text.Parsec.Combinator (choice, endBy, eof, optionMaybe, sepEndBy, sepBy)
import Text.Parsec.Expr -- TODO specific
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim ((<|>), (<?>),
                         many, parse, Parsec, skipMany, token, try)

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
  let tokens' = filter (\(_, tok) -> case tok of
                           TComment _ -> False
                           _ -> True)
                       tokens
  in
  if Path.takeExtension path == ".frm" then
    parse (form path) (Path.toString path) tokens'
  else
    parse (basModule path) (Path.toString path) tokens'

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
                         TKwAs | s == "As" -> Just ()
                         TKwAnd | s == "And" -> Just ()
                         TKwAttribute | s == "Attribute" -> Just ()
                         TKwBoolean | s == "Boolean" -> Just ()
                         TKwCall | s == "Call" -> Just ()
                         TKwCase | s == "Case" -> Just ()
                         TKwDim | s == "Dim" -> Just ()
                         TKwDouble | s == "Double" -> Just ()
                         TKwElse | s == "Else" -> Just ()
                         TKwElseIf | s == "ElseIf" -> Just ()
                         TKwEnd | s == "End" -> Just ()
                         TKwExplicit | s == "Explicit" -> Just ()
                         TKwFalse | s == "False" -> Just ()
                         TKwIf | s == "If" -> Just ()
                         TKwInteger | s == "Integer" -> Just ()
                         TKwOption | s == "Option" -> Just ()
                         TKwPrivate | s == "Private" -> Just ()
                         TKwPublic | s == "Public" -> Just ()
                         TKwSelect | s == "Select" -> Just ()
                         TKwString | s == "String" -> Just ()
                         TKwSub | s == "Sub" -> Just ()
                         TKwThen | s == "Then" -> Just ()
                         TKwTrue | s == "True" -> Just ()
                         _ -> Nothing)
       <?> s

-- | /kwFoo/ parsers parse the keyword /Foo/.
kwAnd :: Parser ()
kwAs :: Parser ()
kwAttribute :: Parser ()
kwBoolean :: Parser ()
kwCall :: Parser ()
kwCase :: Parser ()
kwDim :: Parser ()
kwDouble :: Parser ()
kwElse :: Parser ()
kwElseIf :: Parser ()
kwEnd :: Parser ()
kwExplicit :: Parser ()
kwFalse :: Parser ()
kwIf :: Parser ()
kwInteger :: Parser ()
kwOption :: Parser ()
kwPrivate :: Parser ()
kwPublic :: Parser ()
kwSelect :: Parser ()
kwString :: Parser ()
kwSub :: Parser ()
kwThen :: Parser ()
kwTrue:: Parser ()
kwAnd = kw "And"
kwAs = kw "As"
kwAttribute = kw "Attribute"
kwBoolean = kw "Boolean"
kwCall = kw "Call"
kwCase = kw "Case"
kwDim = kw "Dim"
kwDouble = kw "Double"
kwElse = kw "Else"
kwElseIf = kw "ElseIf"
kwEnd = kw "End"
kwExplicit = kw "Explicit"
kwFalse = kw "False"
kwIf = kw "If"
kwInteger = kw "Integer"
kwOption = kw "Option"
kwPrivate = kw "Private"
kwPublic = kw "Public"
kwSelect = kw "Select"
kwString = kw "String"
kwSub = kw "Sub"
kwThen = kw "Then"
kwTrue = kw "True"

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

booleanLit :: Parser Expr
booleanLit = tokenAccepter (\tok -> case tok of
                               TKwFalse -> Just $ BooleanLit False
                               TKwTrue -> Just $ BooleanLit True
                               _ -> Nothing)
            <?> "boolean literal"

integerLit :: Parser Expr
integerLit = tokenAccepter (\tok -> case tok of
                               TIntegerLit i -> Just $ IntegerLit i
                               _ -> Nothing)
            <?> "integer literal"

doubleLit :: Parser Expr
doubleLit = tokenAccepter (\tok -> case tok of
                              TDoubleLit x -> Just $ DoubleLit x
                              _ -> Nothing)
            <?> "double literal"

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
visibility :: Parser (Maybe Visibility)
visibility =
  optionMaybe main
  where
    main = (kwPrivate *> pure Private) <|> (kwPublic *> pure Public)

-- | Returns a parser for the contents of the file.
basModule :: AbsFile         -- ^ Should be a \".bas\" file module.
           -> Parser Module
basModule path = BasModule (makeRelative rootDir path) <$>
                 (nls *>
                  sepAndMaybeEndByNls1 topLevelStmt <*
                  eof)

-- | Returns a parser for the contents of the file.
form :: AbsFile         -- ^ Should be a \".frm\" file module.
     -> Parser Module
form path = Form (makeRelative rootDir path) <$>
             frmHeader <*>
               (nls *>
               sepAndMaybeEndByNls1 topLevelStmt <*
               eof)

-- FIXME: return type

frmHeader :: Parser ClassHeaderBlock
frmHeader = do
  localKw "VERSION" *> five *> nls1 *>
    main
  where
    localKw s = tokenAccepter (\tok -> case tok of
                                TIdent x | x == mkSymbolName s -> Just ()
                                _ -> Nothing)
    five = tokenAccepter (\tok -> case tok of
                             TDoubleLit x | x == 5.00 -> Just ()
                             _ -> Nothing)
    frxRef = stringLit <* sym ':' <* integerLit -- FIXME return type
    property = do
      name <- ident
      sym '='
      val <- (try frxRef <|> expr) -- FIXME const expression, not expr
      pure (ClassHeaderProp name val)
    propertyBlock = localKw "BeginProperty" *> ident *> nls1 *>
      many (try property *> nls1) *>
      localKw "EndProperty" *>
      pure ClassHeaderPropBlock -- FIXME type
    main = do
      localKw "Begin"
      name1 <- ident
      sym '.'
      name2 <- ident
      name3 <- ident <* nls
      let name =
            (raw name1)
            ++ "."
            ++ (raw name2)
            ++ " "
            ++ (raw name3)
      blocks <- many (((ClassHeaderChild <$> main) <|> propertyBlock <|> property) <* nls1)
      kwEnd
      pure (ClassHeaderBlock name blocks)

attribute :: Parser Attribute
attribute = do
  _ <- kwAttribute *> ident <* sym '=' *>
       expr -- FIXME constant expression only?
  return $ AttributeConstr -- FIXME

dim :: Parser Dim
dim = do
  _ <- kwDim
  things <- commaSep asSomething
  return $ head things -- FIXME

option :: Parser Option
option = do
  _ <- kwOption *> kwExplicit
  return $ OptionConstr -- FIXME

-- FIXME return type (for this an each stmt).
topLevelStmt =
  (Attribute <$> attribute)
  <|> (Dim <$> dim)
  <|> (Option <$> option)
  <|> (Proc <$> sub)

-- | Parser for the Sub statement.  Subs are a kind of procedure.
sub :: Parser Proc
sub = do
  Sub <$> visibility <*> (kwSub *> ident) <*> procArgsDef <* nls1
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
procArgsDef = sym '(' *>
              (commaSep asSomething) -- FIXME return this
              *> sym ')' *> pure []

-- FIXME: return type.  share code between procArgsDef and Dim statement.
asSomething :: Parser Dim
asSomething = do
  x <- (,) <$> ident <*> optionMaybe (kwAs *>
                                      choice [ kwInteger *> pure Integer
                                             , kwDouble *> pure Double
                                             ]
                                      -- FIXME user defined types aren't keywords?
                                     )
  let (n, ty) = x
  pure (DimConstr n ty)

-- TODO: common code between inProcStmt and expr.
callExpr =
  try (CallType <$> ident <* sym '(' <*> commaSep expr <* sym ')')

-- | Parser for statements which can appear inside a procedure.
inProcStmt :: Parser InProcStmt
inProcStmt =
  (Call <$> (kwCall *> ident) <* sym '(' <*> commaSep expr <* sym ')')
  <|> (try (Let <$> ident <* sym '=' <*> expr))
  <|> (Call <$> ident <*> commaSep expr)
  <|> ifStmt
  <|> caseStmt
             <?> "statement"

caseStmt =
  -- FIXME AST fields
  Case <$>
  (kwSelect *> kwCase *> expr) <* nls <*
  many (kwCase <* expr <* nls1 <*
        sepAndEndByNls1 inProcStmt <* nls) <*
  kwEnd <* kwSelect

ifStmt =
  -- TODO could get rid of the tries by refactoring into a common.
  -- cf getting rid of shift/reduce errors.
  blockIf
  <|> inlineIf

blockIf =
  -- TODO: can i get rid of the parentheses inside the If.  maybe
  -- by putting kwIf before If?
  If <$>
      try ((kwIf *> expr) <* kwThen <* nls1) <*
      -- TODO: maybe sepAndEndBy isn't great if I then have to be careful to use nls, instead of nls1 like the surroundings??
      sepAndEndByNls1 inProcStmt <* nls <*
      many elseIf <*
      optionMaybe else' <*
      kwEnd <* kwIf
  where
    elseIf = kwElseIf <* expr <* kwThen <* nls1 <*
             sepAndEndByNls1 inProcStmt <* nls
    else' = kwElse <* nls1 <*
            sepAndEndByNls1 inProcStmt <* nls
inlineIf =
  If <$>
  (kwIf *> expr) <* kwThen <* inProcStmt

-- | Parse an expression.
expr :: Parser Expr
expr =
  buildExpressionParser table term
  <?> "expression"

term :: Parser Expr
term =
  -- parens expr
  (CallExpr <$> callExpr)
  <|> (IdentExpr <$> ident) -- needs to be after call expr.  else implicit let of "foo = bar(args)" won't parse.
  <|> booleanLit
  <|> doubleLit
  <|> integerLit
  <|> stringLit
  <?> "simple expression"

blah x y = x -- TODO create Eq Expr in AST, etc.
table =
  [
    -- FIXME precedence matters, is implicit in order.
    [ Infix (do sym '='; return blah) AssocLeft
    , Infix (do kwAnd; return blah) AssocLeft
    , Infix (do sym '&'; return blah) AssocLeft
    , Infix (do sym '*'; return blah) AssocLeft
    , Infix (do sym '/'; return blah) AssocLeft,
      Infix (do sym '+'; return Add) AssocLeft
    , Infix (do sym '-'; return blah) AssocLeft
    , Infix (do sym '.'; return blah) AssocLeft -- FIXME not at binary operator.
    , Infix (do sym '<'; do sym '>'; return blah) AssocLeft
    ]
  ]
