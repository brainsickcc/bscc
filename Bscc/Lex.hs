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

-- | Lexical analysis.
module Bscc.Lex (lexFileContents) where

import Bscc.Symbol.Name
import Bscc.Token

import Control.Applicative ((<*), (<*>), (*>), (<$>), pure)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Prelude hiding (FilePath)
import System.Path (AbsFile, getPathString)
import Text.Parsec.Char (char, oneOf, satisfy)
import Text.Parsec.Combinator (eof, many1)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim ((<|>), (<?>), getPosition, many, parse, try)
import Text.Parsec.String (Parser)

-- | Lex a file, if successful returning a list of tokens lifted into
-- the `Either' monad.
lexFileContents :: String       -- ^ File contents
                   -> AbsFile
                   -> Either ParseError [Token]
lexFileContents contents path = parse scan (getPathString path) contents

-- | This `Parser' is the /lexer/ or /scanner/.  It tokenizes the input.
scan :: Parser [Token]
scan = do
  mbTs <- many readMbToken
  -- Requiring an EOF forces an error if the scanner can't consume all
  -- of the input.  By setting its description to the empty string, we
  -- prevent ``expecting EOF'' or similar appearing in error messages.
  -- Preventing that is useful because such a lexing error message is
  -- almost certainly incorrect if interpreted by the user as a parsing
  -- error message.  interpreted as a way to
  eof <?> ""
  return $ catMaybes mbTs

-- | Either consume one token's worth of input (returning @`Right'
-- token@) or munch horizontal whitespace (returning @Nothing@).
readMbToken :: Parser (Maybe Token)
readMbToken = do
  -- We automatically add position information to the underlying
  -- scanner.
  pos <- getPosition
  mbTok <- readMbTokenNoPos
  return $ case mbTok of
    Nothing -> Nothing
    Just tok -> Just (pos, tok)

-- | The core of the scanner.
readMbTokenNoPos :: Parser (Maybe TokenNoPos)
readMbTokenNoPos = spaces1 <|> nl
                   <|> identOrKw
                   <|> stringLit
                   <|> symbol

-- | Recognise one or more space character.  Here space is limited to
-- the ASCII space character.  Returns @Nothing@ because such whitespace
-- is not significant to the parser ("Bscc.Parse").
spaces1 :: Parser (Maybe TokenNoPos)
spaces1 = (many1 $ char ' ') *> pure Nothing

-- | Recognise a newline, returning such a token.
nl :: Parser (Maybe TokenNoPos)
nl = satisfy isNl *> pure (Just TNl) <?> "newline"

-- | True if a character is a newline character.
isNl :: Char -> Bool
isNl = (== '\n')

-- | Recognise an identifier or a keyword, returning a keyword or
-- identifier token as appropriate.
identOrKw :: Parser (Maybe TokenNoPos)
identOrKw = do
  s <- identOrKwUnsureWhich <?> "identifier or keyword"
  return $ Just (Map.findWithDefault (TIdent $ mkSymbolName s) (mkSymbolName s)
                 kwMap)

-- | Recognise input which is either an identifier or a keyword.  All
-- keywords are alphabetic and so, without otherwise checking, look like
-- identifiers.
identOrKwUnsureWhich :: Parser String
identOrKwUnsureWhich = (:) <$> initialIdent <*> many nonInitialIdent
  where initialIdent = asciiAlpha
        nonInitialIdent = initialIdent <|> char '_' <|> asciiDigit

        asciiAlpha :: Parser Char
        asciiAlpha = satisfy (\c -> ('a' <= c && c <= 'z') ||
                                    ('A' <= c && c <= 'Z'))

        asciiDigit :: Parser Char
        asciiDigit = oneOf "01234567890"

-- | Map of each keyword to its corresponding token type.
kwMap :: Map.Map SymbolName TokenNoPos
kwMap = Map.fromList (map (\(a, b) -> (mkSymbolName a, b))
                      [("Call", TKwCall),
                       ("End", TKwEnd),
                       ("Sub", TKwSub)])

-- | Recognise a string literal, returning such a token.
stringLit :: Parser (Maybe TokenNoPos)
stringLit = Just . TStringLit <$> (char '"' *> many strBodyChar <* char '"')
  where strBodyChar = strQuoteChar <|> strNormalChar
        strQuoteChar = try (char '"' *> char '"')
        strNormalChar = satisfy $ \c -> (c /= '"') && (not $ isNl c)

-- | Recognise valid symbols.  Does not recognise symbols recognised by
-- other scanners, such as quotation marks which are recognised by the
-- string literal scanner.  Returns the symbol in a token.
symbol :: Parser (Maybe TokenNoPos)
symbol = Just <$> TSym <$> oneOf "(),"
