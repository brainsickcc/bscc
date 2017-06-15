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

-- | Tokens are produced by lexical analysis ("Bscc.Lex") and consumed
-- when parsing ("Bscc.Parse").
module Bscc.Token
       (Token, TokenNoPos (..))
       where

import Bscc.Symbol.Name

import Text.Parsec.Pos (SourcePos)

-- | A token, with position information.
type Token = (SourcePos, TokenNoPos)

-- | A token, without position information.
data TokenNoPos =
  -- | Identifier
  TIdent SymbolName
  | TComment String
  -- | Newline
  | TNl
  -- | Keywords begin here
  | TKwAnd
  | TKwAttribute
  | TKwAs
  | TKwBoolean
  | TKwCall
  | TKwCase
  | TKwDim
  | TKwDouble
  | TKwElse
  | TKwElseIf
  | TKwEnd
  | TKwExplicit
  | TKwFalse
  | TKwIf
  | TKwInteger
  | TKwLong
  | TKwOption
  | TKwPrivate
  | TKwPublic
  | TKwSelect
  | TKwString
  | TKwSub
  | TKwThen
  | TKwTrue
  -- | Literal (except boolean literals which are keywords)s
  | TDoubleLit Double
  | TIntegerLit Integer -- TODO: fixed size int.
  | TLongLit Integer -- Ditto.
  | TStringLit String
  -- | Symbol
  | TSym Char
  deriving (Show, Eq)
