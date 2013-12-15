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

{-# LANGUAGE GADTs #-}

-- | Support for command line arguments parsing, in the style of GCC.
-- Such style is a lot like the X11-style.
--
-- In general, some command line arguments are options; each begins with
-- a hyphen and, depending upon the option type, takes zero or more
-- arguments.  We call the other command line arguments positional
-- arguments.
--
-- Here, an argument of \"--\" does not have any special meaning.  In
-- contrast, many programs interpret it as explicitly separating options
-- from positional arguments.
--
-- We support the long options --help and --version.  Besides this, all
-- other options must be defined by the user of this module.
--
-- In keeping with the GCC style, all user defined options, even long
-- options, should begin with precisely one hyphen.  This is different
-- from the behaviour of the C function @getopt_long@ where long options
-- begin with two hyphens.  GCC's (and our) behaviour even differs from
-- @getopt_long_only@; @-ab@ is not parsed the same as @-a -b@, even
-- when there is no @-ab@ option.  This allows a command line of one
-- argument @-afoo@ to be treated the same as a command line of two
-- arguments @-a foo@.
module Bscc.GccArgParse (argsParse, Arguments (..), OptionArgDecl (..),
                         OptionDecl (..), ParseFailure (..), PosArgs,
                         UnparsedArg, UnparsedArgs)
       where

import qualified Control.Lens as L
import Control.Lens.Operators ((&), (^?), (^?!), (.~), (<>~))
import Data.List (stripPrefix)
import Data.Monoid (Monoid, mempty)
import Safe (headMay)

-- | Positional command line arguments.
type PosArgs = [String]

-- | Result of parsing command line arguments.
data Arguments opts = Help     -- ^ \--help was given
                    | Version  -- ^ \--version was given
                    | -- | Otherwise
                      Normal {
                        -- | Result of parsing the options.  The data
                        -- structure to store this is not specified in
                        -- this module.  The structure best suited to
                        -- store the option parse result depends upon
                        -- all of the possible options, and the arity
                        -- and types of the options.
                        _options :: opts,
                        -- | Positional arguments.
                        _positional :: PosArgs
                        }
                    deriving (Eq, Show)

positional :: L.Lens' (Arguments opts) PosArgs
positional = L.lens _positional $ \s a -> s { _positional = a }

options :: L.Lens' (Arguments opts) opts
options = L.lens _options $ \s a -> s { _options = a }

-- | Input parameter should represent the result of successfully parsing
-- zero command line options.  Returns a record which represents the
-- result of successfully parsing zero command line arguments.  See
-- `options'.
defaultArguments :: Monoid opts => Arguments opts
defaultArguments = Normal { _options = mempty,
                            _positional = [] }

-- | Command line arguments which have not yet been parsed.
type UnparsedArg = String
type UnparsedArgs = [UnparsedArg]

-- | Declares an option such that we can parse the option with
-- `argsParse', suitably handling its parameters.  @opts@ is per
-- `options'.
--
-- Only define `OptionDecl's for `Normal' options.
data OptionDecl opts = OptionDecl { str :: String,
                                    -- ^ String representation of the
                                    -- option, excluding any parameters.
                                    -- For example, @\"-o\"@.
                                    argDecl :: OptionArgDecl opts }

-- | Declares the number of arguments for an option.  Additionally,
-- describes the option (@opt@) in a structure of options (@opts@) to
-- update via a mappend when the option is parsed.
data OptionArgDecl opts where
  -- | The option takes no arguments.  (The effect of the option must be
  -- implicit in the fact that the option can be given.)
  DeclNoArgs :: Monoid opt => opt -> L.ASetter' opts opt -> OptionArgDecl opts
  -- | The option takes one \"squishable\" argument.  By squishable we
  -- mean the argument \"oobar\" need not be preceded by whitespace
  -- after writing the option \"-f\", like this: \"-foobar\".  However,
  -- the whitespace can be given, and indeed \"-foobar\" and \"-f
  -- oobar\" are equivalent.  (In these examples, imagine the command
  -- line options were written in a shell, without the quotes).
  DeclOneSquishableArg :: Monoid opt =>
                          (UnparsedArg -> opt)
                          -> L.ASetter' opts opt
                          -> OptionArgDecl opts

-- | Parse failure.
data ParseFailure optDecl
     = MissingArgumentToOption optDecl
     | UnrecognizedOption UnparsedArg

-- | Parse command line arguments.
argsParse :: Monoid opts =>
             [OptionDecl opts] -- ^ List of declarations of the options
                               --   we support.
            -> UnparsedArgs    -- ^ Command line arguments to parse.
            -- | Parse result.
            -> Either (ParseFailure (OptionDecl opts)) (Arguments opts)
argsParse optionDecls = argsParse' optionDecls defaultArguments

argsParse' :: [OptionDecl opts]
              -> Arguments opts
              -> UnparsedArgs
              -> Either (ParseFailure (OptionDecl opts)) (Arguments opts)
argsParse' _ parsed [] = Right parsed
argsParse' optionDecls parsed (headArg:tailArgs) =
  case (headMay headArg) of
    Just '-' -> case headArg of
      -- Argument begins with a hyphen; treat it as an option.
      "--help" -> Right Help
      "--version" -> if "--help" `elem` tailArgs
                     then Right Help
                     else Right Version
      _ -> do
        -- Try the optionDecl-defined options to see if one will accept
        -- this option as its own.
        let tryParse = tryOption headArg tailArgs $ parsed ^?! options
            attemptedParses = map tryParse optionDecls
        case attemptedParses ^? L.traverse . L._Right of
          Just (unparsed', updatedOptions) ->
            argsParse' optionDecls parsed' unparsed'
            where parsed' = parsed & options .~ updatedOptions
          Nothing -> case attemptedParses ^? L.traverse . L._Left of
            Just err -> Left err
            Nothing -> Left $ UnrecognizedOption headArg
    -- Take as a positional arg.
    Just _ -> argsParse' optionDecls (parsed & positional <>~ [headArg]) tailArgs
    Nothing -> error "argsParse': empty headArg"


-- | Attempt to parse an option.
tryOption  :: UnparsedArg
              -- ^ Head of the unparsed arguments.
              -> UnparsedArgs
              -- ^ Tail.
              -> opts
              -- ^ Data structure representing the result of the options
              -- parsed so far [cf. `options']).
              -> OptionDecl opts
              -- ^ Declaration of the option and its effect.
              -> Either (ParseFailure (OptionDecl opts)) (UnparsedArgs, opts)
              -- ^ If parsing is successful, we essentially return the
              --   first parameter updated, as the `Right'.  The updated
              --   `UnparsedArgs' is necessarily shorter than as
              --   inputted.
tryOption hdUnparsed tlUnparsed parsedOptions option = case (argDecl option) of
  DeclNoArgs implicitOptionVal optionSetter ->
    if hdUnparsed == str option
       -- For example, parsing "-v".
    then Right (tlUnparsed, parsedOptions & optionSetter <>~ implicitOptionVal)
    else Left $ UnrecognizedOption hdUnparsed
  DeclOneSquishableArg parseOption setOption ->
    if hdUnparsed == str option
       -- For example, we're trying to parse a command line of "-o foo"
       -- (where the "-o foo" is interpreted as if it were without the
       -- quotes and written in the shell).
    then let (maybeHdTlUnparsed, tlTlUnparsed) = splitAt1May tlUnparsed
         in case maybeHdTlUnparsed of
           Just hdTlUnparsed ->
             Right (tlTlUnparsed,
                    parsedOptions & setOption <>~ parseOption hdTlUnparsed)
           Nothing -> Left $ MissingArgumentToOption option
    else case stripPrefix (str option) hdUnparsed of
      -- For example, parse "-ofoo" identically to the "-o foo" above.
      Just suffix -> Right (tlUnparsed,
                            parsedOptions & setOption <>~ parseOption suffix)
      -- This isn't our argument to accept.
      Nothing -> Left $ UnrecognizedOption hdUnparsed

-- | Returns the head and the tail.  Similar to `splitAt 1'.  Total.
splitAt1May :: [a] -> (Maybe a, [a])
splitAt1May [] = (Nothing, [])
splitAt1May (x:xs) = (Just x, xs)
